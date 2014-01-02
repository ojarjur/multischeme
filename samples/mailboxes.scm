;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample code showing how to define fixed-size blocking queues. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type
  <mailbox-record>
  (make-mailbox-record store size read-position insert-position)
  mailbox?
  (store mailbox-store)
  (size mailbox-size)
  (read-position mailbox-read-position set-mailbox-read-position!)
  (insert-position mailbox-insert-position set-mailbox-insert-position!))
(define (make-mailbox . args)
  (let ((size (if (pair? args) (car args) 1)))
    (make-mailbox-record (make-vector size) size 0 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Change the mailbox-empty? and mailbox-full? predicates to           ;;
;; be syntax rules, so that they can be included as part of an atomic block. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mailbox-empty? mailbox)
  (= (mailbox-read-position mailbox)
     (mailbox-insert-position mailbox)))
(define (mailbox-full? mailbox)
  (= (- (mailbox-insert-position mailbox)
        (mailbox-size mailbox))
     (mailbox-read-position mailbox)))
(define (mailbox-send mailbox message)
  (let ((store (mailbox-store mailbox))
        (read-position (mailbox-read-position mailbox))
        (insert-position (mailbox-insert-position mailbox))
        (size (mailbox-size mailbox)))
    (if (= (- insert-position size) read-position)
        ;; The fixed size buffer is full. Wait and check again.
        (mailbox-send mailbox message)
        ;; Send a message, the entire following block is atomic,
        ;; so we don't have to worry about read-modify-write conflicts.
        (begin (set-mailbox-insert-position! mailbox (+ insert-position 1))
               (vector-set! store (remainder insert-position size) message)
               mailbox))))
(define (mailbox-poll mailbox message-handler empty-thunk)
  (let ((store (mailbox-store mailbox))
        (read-position (mailbox-read-position mailbox))
        (insert-position (mailbox-insert-position mailbox))
        (size (mailbox-size mailbox)))
    (if (= read-position insert-position)
        ;; The fixed size buffer is empty.
        (empty-thunk)
        ;; Send a message to the handler. The entire following block (until
        ;; the final tail-call) is atomic, so we don't have to worry about
        ;; read-modify-write conflicts.
        (begin (set-mailbox-read-position! mailbox (+ read-position 1))
               (message-handler (vector-ref store
                                            (remainder read-position size)))))))
(define (mailbox-receive mailbox)
  (mailbox-poll mailbox
                (lambda (message) message)
                (lambda () (mailbox-receive mailbox))))
(define (mailbox-select mailbox . mailboxes)
  (define (recursive-select m ms)
    (mailbox-poll m
                  (lambda (message) message)
                  (lambda ()
                    (if (null? ms)
                        (recursive-select mailbox mailboxes)
                        (recursive-select (car mailboxes)
                                          (cdr mailboxes))))))
  (recursive-select mailbox mailboxes))
