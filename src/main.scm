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

(define (compile-statement statement)
  (transform-statement (rewrite statement)))
(define (read-and-compile input-port)
  (let ((expr (read input-port)))
    (if (not (eof-object? expr))
        (begin (if (and (pair? expr) (eq? (car expr) 'load))
                   (call-with-input-file (cadr expr) read-and-compile)
                   (write (compile-statement expr)))
               (newline)
               (read-and-compile input-port)))))
(define (compile input-port)
  (begin (for-each (lambda (statement)
                     (begin (write statement)
                            (newline)))
		   (if overload-primitives?
		       (append primitives
			       multitasking-definitions)
		       multitasking-definitions))
         (for-each (lambda (statement)
                     (begin (write (compile-statement statement))
                            (newline)))
                   builtins)
         (read-and-compile input-port)
         (exit)))
(compile (current-input-port))
