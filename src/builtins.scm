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
;; TODO: This should be extended to include read, eval, and the various  ;;
;; environment procedures.                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define builtins
  '((define (reverse elements)
      (define (rreverse remaining-elements result)
        (if (pair? remaining-elements)
          (rreverse
            (cdr remaining-elements)
            (cons (car remaining-elements) result))
          result))
      (rreverse elements '()))
    (define (map proc list . lists)
      (if (pair? list)
        (cons (apply proc (cons (car list) (map car lists)))
              (apply map (cons proc (cons (cdr list) (map cdr lists)))))
        '()))
    (define (for-each proc list . lists)
      (if (pair? list)
        (begin
          (apply proc (cons (car list) (map car lists)))
          (apply for-each (cons proc (cons (cdr list) (map cdr lists)))))))
    (define (call-with-input-file filename callback)
      (let ((port (open-input-file filename)))
        (let ((value (callback port))) (begin (close-input-port port) value))))
    (define (call-with-output-file filename callback)
      (let ((port (open-output-file filename)))
        (let ((value (callback port)))
          (begin (close-output-port port) value))))
    (define (newline . args)
      (write-char
        #\newline
        (if (pair? args) (car args) (current-output-port))))
    (define (read-string k . args)
      (let ((port (if (null? args) (current-input-port) (car args))))
        (define (recursive-read k chars)
          (if (> k 0)
              (let ((char (read-char port)))
                (if (eof-object? char)
                    (if (null? chars) char (list->string (reverse chars)))
                    (recursive-read (- k 1) (cons char chars))))
              (list->string (reverse chars))))
        (recursive-read k '())))
    (define (read-line . args)
      (let ((port (if (null? args) (current-input-port) (car args))))
        (define (recursive-read chars)
          (let ((char (read-char port)))
            (cond ((eof-object? char)
                   (if (null? chars) char (list->string (reverse chars))))
                  ((eq? char #\newline)
                   (list->string (reverse chars)))
                  (#t (recursive-read (cons char chars))))))
        (recursive-read '())))))
