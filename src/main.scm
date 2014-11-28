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
(load "src/builtins.scm")
(load "src/desugar.scm")
(load "src/multitask.scm")

(define (write-with-newline expr) (begin (write expr) (newline)))
(define rewrite-rules (make-rewrite-rules '()))
(define defined-globals
  (append
    extended-primitives
    (map (lambda (builtin) (car (car (cdr builtin)))) builtins)))
(define (compile-statement get-next-symbol)
  (let ((transform (transform-statement get-next-symbol)))
    (lambda (statement)
      (rewrite-rules
        statement
        (lambda (rewritten-statement)
          (write-with-newline
            (transform
              rewritten-statement
              defined-globals
              get-next-symbol)))))))
(define (load-statements input-port)
  (let ((expr (read input-port)))
    (if (not (eof-object? expr))
      (if (and (pair? expr) (eq? (car expr) 'load))
        (append
          (call-with-input-file (cadr expr) load-statements)
          (load-statements input-port))
        (begin
          (if (and (pair? expr) (eq? (car expr) 'define))
            (set! defined-globals
              (cons (if (pair? (cadr expr)) (caadr expr) (cadr expr))
                    defined-globals)))
          (cons expr (load-statements input-port))))
      '())))
(define (symbols-starting-with prefix expr)
  (define prefix-length (string-length prefix))
  (define (matches? symbol)
    (let* ((symbol-string (symbol->string symbol)))
      (string=?
        (substring
          symbol-string
          0
          (min (string-length symbol-string) prefix-length))
        prefix)))
  (define (recursive-find expr result)
    (cond ((and (symbol? expr) (matches? expr) (not (member expr result)))
           (cons expr result))
          ((pair? expr)
           (recursive-find (car expr) (recursive-find (cdr expr) result)))
          (#t result)))
  (recursive-find expr '()))
(define (make-get-next-symbol prefix excluded-symbols)
  (let ((next-id 0))
    (define (get-next-symbol)
      (let ((symbol
              (string->symbol
                (string-append prefix (number->string next-id)))))
        (begin
          (set! next-id (+ next-id 1))
          (if (member symbol excluded-symbols) (get-next-symbol) symbol))))
    get-next-symbol))
(define (compile input-port)
  (let* ((statements (append builtins (load-statements input-port)))
         (symbol-prefix "_s_")
         (conflicting-symbols (symbols-starting-with symbol-prefix statements))
         (get-next-symbol
           (make-get-next-symbol symbol-prefix conflicting-symbols))
         (compiler (compile-statement get-next-symbol)))
    (begin
      (for-each write-with-newline multitasking-definitions)
      (for-each compiler statements)
      (exit))))

(compile (current-input-port))
