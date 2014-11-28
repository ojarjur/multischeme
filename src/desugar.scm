;; Copyright 2013 Omar Jarjur
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper procedures for simplifying statements before cps-transforming them
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (rewrite-named-let name bindings rewritten-body)
  (cons `(letrec ((,name (lambda ,(map car bindings) ,rewritten-body))) ,name)
        (map (lambda (binding) (rewrite (cadr binding))) bindings)))
(define (rewrite-let bindings rewritten-body)
  `(let (unquote
         (map (lambda (binding) `(,(car binding) ,(rewrite (cadr binding))))
              bindings))
     ,rewritten-body))
(define (rewrite-let* bindings rewritten-body)
  (if (pair? bindings)
    `(let ((,(caar bindings) ,(rewrite (cadar bindings))))
       ,(rewrite-let* (cdr bindings) rewritten-body))
    rewritten-body))
(define (rewrite-letrec bindings rewritten-body)
  `(letrec (unquote
            (map (lambda (binding) `(,(car binding) ,(rewrite (cadr binding))))
                 bindings))
     ,rewritten-body))
(define (rewrite-letrec* bindings rewritten-body)
  (if (pair? bindings)
    `(letrec ((,(caar bindings) ,(rewrite (cadar bindings))))
       ,(rewrite-letrec* (cdr bindings) rewritten-body))
    rewritten-body))
(define (rewrite-cond cases)
  (cond ((not (pair? cases)) '(begin))
        ((or (eq? (caar cases) 'else) (eq? (caar cases) #t))
         (cons 'begin (map rewrite (cdar cases))))
        ((null? (cdar cases))
         `(let ((test ,(rewrite (caar cases)))
                (otherwise (lambda () ,(rewrite-cond (cdr cases)))))
            (if test test (otherwise))))
        ((eq? (cadar cases) '=>)
         `(let ((test ,(rewrite (caar cases)))
                (then-thunk (lambda () ,(rewrite (caddar cases))))
                (else-thunk (lambda () ,(rewrite-cond (cdr cases)))))
            (if test ((then-thunk) test) (else-thunk))))
        (#t
         `(if ,(rewrite (caar cases))
            ,(cons 'begin (map rewrite (cdar cases)))
            ,(rewrite-cond (cdr cases))))))
(define (rewrite-case key cases)
  (define (rewrite-cases cases next-id cont)
    (if (pair? cases)
      (rewrite-cases
        (cdr cases)
        (+ next-id 1)
        (lambda (bindings code)
          (let ((thunk-id
                  (string->symbol
                    (string-append "t_" (number->string next-id)))))
            (if (eq? (caar cases) 'else)
              (cont `((,thunk-id
                       (lambda () ,(cons 'begin (map rewrite (cdar cases))))))
                    `(,thunk-id))
              (cont (cons `(,thunk-id
                            (lambda ()
                              ,(cons 'begin (map rewrite (cdar cases)))))
                          bindings)
                    `(if (memv key ',(caar cases)) (,thunk-id) ,code))))))
      (cont '() '(begin))))
  (rewrite-cases
    cases
    0
    (lambda (bindings code)
      `(let ,(cons `(key ,(rewrite key)) bindings) ,code))))
(define (free-symbol . args)
  (letrec ((symbol-in?
             (lambda (symbol expr)
               (if (pair? expr)
                 (or (symbol-in? symbol (car expr))
                     (symbol-in? symbol (cdr expr)))
                 (eq? symbol expr))))
           (find-symbol
             (lambda (next-id)
               (let* ((symbol-name
                        (string-append "s_" (number->string next-id)))
                      (symbol (string->symbol symbol-name)))
                 (if (symbol-in? symbol args)
                   (find-symbol (+ next-id 1))
                   symbol)))))
    (find-symbol 0)))
(define (rewrite-do vars test commands)
  (let ((params (map car vars))
        (first-args (map (lambda (var) (rewrite (cadr var))) vars))
        (update-args
          (map (lambda (var)
                 (if (pair? (cddr var)) (rewrite (caddr var)) (car var)))
               vars))
        (loop-var (free-symbol vars test commands)))
    `(letrec ((,loop-var
               (lambda ,params
                 (if ,(rewrite (car test))
                   ,(cons 'begin (map rewrite (cdr test)))
                   ,(cons 'begin
                          (append
                            (map rewrite commands)
                            `(,(cons loop-var update-args))))))))
       ,(cons loop-var first-args))))
(define (rewrite-and tests)
  (if (pair? tests)
    (if (pair? (cdr tests))
      `(let ((first ,(rewrite (car tests)))
             (rest (lambda () ,(rewrite-and (cdr tests)))))
         (if first (rest) first))
      (rewrite (car tests)))
    '#t))
(define (rewrite-or tests)
  (if (pair? tests)
    `(let ((first ,(rewrite (car tests)))
           (rest (lambda () ,(rewrite-or (cdr tests)))))
       (if first first (rest)))
    '#f))
(define (rewrite-quasiquote expr depth)
  (cond ((= depth 0) (rewrite expr))
        ((vector? expr)
         (let ((rewritten-list (rewrite-quasiquote (vector->list expr) depth)))
           (if (and (pair? rewritten-list) (eq? (car rewritten-list) 'quote))
             (list 'quote (list->vector (cadr rewritten-list)))
             (list 'list->vector rewritten-list))))
        ((not (pair? expr)) (list 'quote expr))
        ((eq? (car expr) 'quasiquote)
         (rewrite-quasiquote (cadr expr) (+ depth 1)))
        ((eq? (car expr) 'unquote)
         (rewrite-quasiquote (cadr expr) (- depth 1)))
        ((and (pair? (car expr))
              (eq? (caar expr) 'unquote-splicing)
              (= depth 1))
         `(append
            ,(rewrite (cadar expr))
            ,(rewrite-quasiquote (cdr expr) depth)))
        (#t
         (let ((head (rewrite-quasiquote (car expr) depth))
               (tail (rewrite-quasiquote (cdr expr) depth)))
           (if (and (pair? head)
                    (pair? tail)
                    (eq? (car head) 'quote)
                    (eq? (car tail) 'quote))
             (list 'quote (cons (cadr head) (cadr tail)))
             (list 'cons head tail))))))
(define (get-definitions exprs definitions bodies return)
  (if (pair? exprs)
    (if (and (pair? (car exprs)) (eq? (caar exprs) 'define))
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
      ,(rewrite (append `(lambda ,(cdadr definition)) (cddr definition))))
    `(,(cadr definition) ,(rewrite (caddr definition)))))
(define (rewrite-statements statements)
  (get-definitions
    statements
    '()
    '()
    (lambda (definitions bodies)
      (if (pair? definitions)
        `(letrec ,(map get-binding definitions)
           ,(cons 'begin (map rewrite bodies)))
        (cons 'begin (map rewrite bodies))))))
(define (rewrite expr)
  (cond ((not (pair? expr)) expr)
        ((eq? (car expr) 'quote) expr)
        ((eq? (car expr) 'quasiquote) (rewrite-quasiquote (cadr expr) 1))
        ((eq? (car expr) 'let)
         (if (symbol? (cadr expr))
           (rewrite-named-let
             (cadr expr)
             (caddr expr)
             (rewrite-statements (cdddr expr)))
           (rewrite-let (cadr expr) (rewrite-statements (cddr expr)))))
        ((eq? (car expr) 'letrec)
         (rewrite-letrec (cadr expr) (rewrite-statements (cddr expr))))
        ((eq? (car expr) 'let*)
         (rewrite-let* (cadr expr) (rewrite-statements (cddr expr))))
        ((eq? (car expr) 'letrec*)
         (rewrite-letrec* (cadr expr) (rewrite-statements (cddr expr))))
        ((eq? (car expr) 'cond) (rewrite-cond (cdr expr)))
        ((eq? (car expr) 'case) (rewrite-case (cadr expr) (cddr expr)))
        ((eq? (car expr) 'do)
         (rewrite-do (cadr expr) (caddr expr) (cdddr expr)))
        ((eq? (car expr) 'and) (rewrite-and (cdr expr)))
        ((eq? (car expr) 'or) (rewrite-or (cdr expr)))
        ((eq? (car expr) 'define)
         (if (pair? (cadr expr))
           `(define ,(caadr expr)
              ,(rewrite (append `(lambda ,(cdadr expr)) (cddr expr))))
           `(define ,(cadr expr) ,(rewrite (caddr expr)))))
        ((eq? (car expr) 'set!) `(set! ,(cadr expr) ,(rewrite (caddr expr))))
        ((eq? (car expr) 'begin) (rewrite-statements (cdr expr)))
        ((eq? (car expr) 'lambda)
         `(lambda ,(cadr expr) ,(rewrite-statements (cddr expr))))
        ((eq? (car expr) 'if)
         (if (pair? (cdddr expr))
           `(if ,(rewrite (cadr expr))
              ,(rewrite (caddr expr))
              ,(rewrite (cadddr expr)))
           `(if ,(rewrite (cadr expr)) ,(rewrite (caddr expr)) (begin))))
        (#t (map rewrite expr))))
(define (make-rewrite-rules syntax-bindings)
  (lambda (statement return)
    (if (and (pair? statement) (eq? (car statement) 'define-syntax))
      (set! syntax-bindings
        (compile-syntax-rules (cadr statement) (caddr statement)))
      (return (rewrite statement)))))
