;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample code showing how to define fixed-size blocking queues. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Switch to using a record type once the multitasking     ;;
;; transformation supports them.                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-mailbox . args)
  (let ((size (if (pair? args) (car args) 1))
        (read-start 0)
        (insert-position 0))
    (list (make-vector size) read-start insert-position size)))
(define (mailbox-empty? mailbox)
  (let ((read-start (cadr mailbox))
        (insert-position (caddr mailbox)))
    (= read-start insert-position)))
(define (mailbox-full? mailbox)
  (let ((read-start (cadr mailbox))
        (insert-position (caddr mailbox))
        (size (cadddr mailbox)))
    (= (- insert-position size) read-start)))
(define (send-message mailbox message)
  (let ((store (car mailbox))
        (read-start (cadr mailbox))
        (insert-position (caddr mailbox))
        (size (cadddr mailbox)))
    (if (= (- insert-position size) read-start)
        ;; The fixed size buffer is full. Wait and check again.
        (send-message mailbox message)
        ;; Send a message, the entire following block is atomic,
        ;; so we don't have to worry about read-modify-write conflicts.
        (begin (set-car! (cddr mailbox) (+ insert-position 1))
               (vector-set! store (remainder insert-position size) message)
               mailbox))))
(define (receive-message mailbox)
  (let ((store (car mailbox))
        (read-start (cadr mailbox))
        (insert-position (caddr mailbox))
        (size (cadddr mailbox)))
    (if (= read-start insert-position)
        ;; The fixed size buffer is empty. Wait and check again.
        (receive-message mailbox)
        ;; Send a message, the entire following block is atomic,
        ;; so we don't have to worry about read-modify-write conflicts.
        (begin (set-car! (cdr mailbox) (+ read-start 1))
               (vector-ref store (remainder read-start size))))))
