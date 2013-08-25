;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in procedures.                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file defines standard procedures that are expected in any Scheme ;;
;; implementation, but which should not be treated as atomic primitives. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define builtins
  '((define (map proc elements)
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