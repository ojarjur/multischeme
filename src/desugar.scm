;; Copyright 2013 Omar Jarjur
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper procedures for simplifying statements before cps-transforming them ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (rewrite-let bindings body)
  `(let (unquote
         (map (lambda (binding)
                `(,(car binding)
                  ,(rewrite (cadr binding))))
              bindings))
     ,(rewrite body)))
(define (rewrite-letrec bindings body)
  `(letrec (unquote
            (map (lambda (binding)
                   `(,(car binding)
                     ,(rewrite (cadr binding))))
                 bindings))
     ,(rewrite body)))
(define (rewrite-cond cases)
  (cond ((not (pair? cases)) (quote (quote ())))
        ((or (eq? (caar cases) (quote default))
             (eq? (caar cases) #t))
         (rewrite (cadar cases)))
        (#t
         `(if ,(rewrite (caar cases))
            ,(rewrite (cadar cases))
            ,(rewrite-cond (cdr cases))))))
(define (rewrite-and tests)
  (if (pair? tests)
    `(if ,(rewrite (car tests))
       ,(rewrite-and (cdr tests))
       #f)
    '#t))
(define (rewrite-or tests)
  (if (pair? tests)
    `(if ,(rewrite (car tests))
       #t
       ,(rewrite-or (cdr tests)))
    '#f))
(define (rewrite-quasiquote expr depth)
  (letrec ((rewrite-parts
             (lambda (expr)
               (if (pair? expr)
                 (let ((head (rewrite-quasiquote (car expr) depth))
                       (tail (rewrite-parts (cdr expr))))
                   (if (and (pair? head)
                            (pair? tail)
                            (eq? (car head) (quote quote))
                            (eq? (car tail) (quote quote)))
                     (list 'quote
                           (cons (cadr head) (cadr tail)))
                     (list (quote cons) head tail)))
                 (list (quote quote) expr)))))
    (cond ((not (pair? expr)) (list (quote quote) expr))
          ((eq? (car expr) (quote quasiquote))
           (rewrite-quasiquote (cadr expr) (+ depth 1)))
          ((eq? (car expr) (quote unquote))
           (if (= depth 0)
             (rewrite (cadr expr))
             (rewrite-quasiquote (cadr expr) (- depth 1))))
          (#t (rewrite-parts expr)))))
(define (get-definitions exprs definitions bodies return)
  (if (pair? exprs)
    (if (and (pair? (car exprs))
             (eq? (caar exprs) (quote define)))
      (get-definitions
        (cdr exprs)
        (cons (car exprs) definitions)
        bodies
        return)
      (get-definitions
        (cdr exprs)
        definitions
        (cons (car exprs) bodies)
        return))
    (return (reverse definitions) (reverse bodies))))
(define (get-binding definition)
  (if (pair? (cadr definition))
    `(,(caadr definition)
      ,(rewrite
         (append
           `(lambda (unquote (cdadr definition)))
           (cddr definition))))
    `(,(cadr definition)
      ,(rewrite (caddr definition)))))
(define (rewrite-lambda-body statements)
  (get-definitions
    statements
    '()
    '()
    (lambda (definitions bodies)
      (if (pair? definitions)
        `(letrec ,(map get-binding definitions)
           ,(cons (quote begin) (map rewrite bodies)))
        (cons (quote begin) (map rewrite bodies))))))
(define (rewrite expr)
  (cond ((not (pair? expr)) expr)
        ((eq? (car expr) (quote quote)) expr)
        ((eq? (car expr) (quote quasiquote))
         (rewrite-quasiquote (cadr expr) 0))
        ((eq? (car expr) (quote let))
         (rewrite-let (cadr expr) (caddr expr)))
        ((eq? (car expr) (quote letrec))
         (rewrite-letrec (cadr expr) (caddr expr)))
        ((eq? (car expr) (quote cond))
         (rewrite-cond (cdr expr)))
        ((eq? (car expr) (quote and))
         (rewrite-and (cdr expr)))
        ((eq? (car expr) (quote or))
         (rewrite-or (cdr expr)))
        ((eq? (car expr) (quote define))
         (if (pair? (cadr expr))
           `(define ,(caadr expr)
              ,(rewrite
                 (append
                   `(lambda (unquote (cdadr expr)))
                   (cddr expr))))
           `(define ,(cadr expr)
              ,(rewrite (caddr expr)))))
        ((eq? (car expr) (quote set!))
         `(set! ,(cadr expr)
            ,(rewrite (caddr expr))))
        ((eq? (car expr) (quote begin))
         (cons (quote begin) (map rewrite (cdr expr))))
        ((eq? (car expr) (quote lambda))
         `(lambda ,(cadr expr)
            ,(rewrite-lambda-body (cddr expr))))
        ((eq? (car expr) (quote if))
         (if (pair? (cdddr expr))
           `(if ,(rewrite (cadr expr))
              ,(rewrite (caddr expr))
              ,(rewrite (cadddr expr)))
           `(if ,(rewrite (cadr expr))
              ,(rewrite (caddr expr))
              '())))
        (#t (map rewrite expr))))
