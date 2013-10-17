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
(define defined-globals
  (append
    extended-primitives
    (map (lambda (builtin) (cadr (rewrite builtin))) builtins)))
(define (compile-statement statement)
  (write-with-newline (transform-statement statement defined-globals)))
(define (load-statements input-port)
  (let ((expr (read input-port)))
    (if (not (eof-object? expr))
      (if (and (pair? expr) (eq? (car expr) 'load))
        (append
          (call-with-input-file (cadr expr) load-statements)
          (load-statements input-port))
        (let ((rewritten-statement (rewrite expr)))
          (begin
            (if (eq? (car rewritten-statement) 'define)
              (set! defined-globals
                (cons (cadr rewritten-statement) defined-globals)))
            (cons rewritten-statement (load-statements input-port)))))
      '())))
(define (compile input-port)
  (begin
    (for-each write-with-newline multitasking-definitions)
    (for-each
      compile-statement
      (append (map rewrite builtins) (load-statements input-port)))
    (exit)))

(compile (current-input-port))
