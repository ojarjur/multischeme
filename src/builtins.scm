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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in procedures.                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file defines standard procedures that are expected in any Scheme ;;
;; implementation, but which should not be treated as atomic primitives. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define builtins
  '((define (reverse elements)
      (define (rreverse remaining-elements result)
        (if (pair? remaining-elements)
            (rreverse (cdr remaining-elements)
                      (cons (car remaining-elements) result))
            result))
      (rreverse elements '()))
    (define (map proc elements)
      (if (pair? elements)
          (cons (proc (car elements))
                (map proc (cdr elements)))
          '()))
    (define (for-each proc elements)
      (if (pair? elements)
          (begin (proc (car elements))
                 (for-each proc (cdr elements)))
          '()))
    (define (call-with-input-file filename callback)
      (let ((port (open-input-file filename)))
        (let ((value (callback port)))
          (begin (close-input-port port)
                 value))))
    (define (call-with-output-file filename callback)
      (let ((port (open-output-file filename)))
        (let ((value (callback port)))
          (begin (close-output-port port)
                 value))))))