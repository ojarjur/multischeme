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
  `(let ,(map (lambda (binding)
                `(,(car binding) ,(rewrite (cadr binding))))
              bindings)
     ,(rewrite body)))
(define (rewrite-cond cases)
  (cond ((not (pair? cases))
         ''())
        ((or (eq? (caar cases) 'default)
             (eq? (caar cases) #t))
         (rewrite (cadar cases)))
        (#t `(if ,(rewrite (caar cases))
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
(define (rewrite-quasiquote-parts expr depth)
  (if (pair? expr)
      (let ((head (rewrite-quasiquote (car expr) depth))
            (tail (rewrite-quasiquote-parts (cdr expr) depth)))
        (if (and (pair? head)
                 (pair? tail)
                 (eq? (car head) 'quote)
                 (eq? (car tail) 'quote))
            (list 'quote (cons (cadr head) (cadr tail)))
            (list 'cons head tail)))
      (list 'quote expr)))
(define (rewrite-quasiquote expr depth)
  (cond ((not (pair? expr)) (list 'quote expr))
        ((eq? (car expr) 'quasiquote)
         (rewrite-quasiquote (cadr expr) (+ depth 1)))
        ((eq? (car expr) 'unquote)
         (if (= depth 0)
             (rewrite (cadr expr))
             (rewrite-quasiquote (cadr expr) (- depth 1))))
        (#t (rewrite-quasiquote-parts expr depth))))
(define (rewrite expr)
  (cond ((not (pair? expr)) expr)
        ((eq? (car expr) 'quote) expr)
        ((eq? (car expr) 'quasiquote)
         (rewrite-quasiquote (cadr expr) 0))
        ((eq? (car expr) 'let)
         (rewrite-let (cadr expr) (caddr expr)))
        ((eq? (car expr) 'cond) (rewrite-cond (cdr expr)))
        ((eq? (car expr) 'and) (rewrite-and (cdr expr)))
        ((eq? (car expr) 'or) (rewrite-or (cdr expr)))
        ((eq? (car expr) 'define)
         (if (pair? (cadr expr))
             `(define ,(caadr expr)
                (lambda ,(cdadr expr) ,(rewrite (caddr expr))))
             `(define ,(cadr expr) ,(rewrite (caddr expr)))))
        ((eq? (car expr) 'set!)
         `(set! ,(cadr expr) ,(rewrite (caddr expr))))
        ((eq? (car expr) 'begin)
         (cons 'begin (map rewrite (cdr expr))))
        ((eq? (car expr) 'lambda)
         `(lambda ,(cadr expr) ,(rewrite (caddr expr))))
        ((eq? (car expr) 'if)
         (if (pair? (cdddr expr))
             `(if ,(rewrite (cadr expr))
                  ,(rewrite (caddr expr))
                  ,(rewrite (cadddr expr)))
             `(if ,(rewrite (cadr expr))
                  ,(rewrite (caddr expr))
                  '())))
        (#t (map rewrite expr))))
