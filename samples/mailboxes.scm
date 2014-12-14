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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: the mailbox-empty? and mailbox-full? predicates are syntax rules, ;;
;; so that they can be included as part of an atomic block.                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax mailbox-empty?
  (syntax-rules ()
    ((mailbox-empty? mailbox)
     (= (mailbox-read-position mailbox)
        (mailbox-insert-position mailbox)))))
(define-syntax mailbox-full?
  (syntax-rules ()
    ((mailbox-full? mailbox)
     (= (- (mailbox-insert-position mailbox)
           (mailbox-size mailbox))
        (mailbox-read-position mailbox)))))
(define (mailbox-send mailbox message)
  (if (mailbox-full? mailbox)
      ;; The fixed size buffer is full. Wait and check again.
      (mailbox-send mailbox message)
      ;; Send a message, this method's entire body is atomic,
      ;; so we don't have to worry about read-modify-write conflicts.
      (let ((store (mailbox-store mailbox))
            (insert-position (mailbox-insert-position mailbox))
            (size (mailbox-size mailbox)))
        (begin (set-mailbox-insert-position! mailbox (+ insert-position 1))
               (vector-set! store (remainder insert-position size) message)
               mailbox))))
(define (mailbox-poll mailbox message-handler empty-thunk)
  (if (mailbox-empty? mailbox)
      ;; The fixed size buffer is empty.
      (empty-thunk)
      ;; Send a message to the handler. Other than the final tail-call,
      ;; this method's entire body is atomic, so we don't have to worry
      ;; about read-modify-write conflicts.
      (let ((store (mailbox-store mailbox))
            (read-position (mailbox-read-position mailbox))
            (size (mailbox-size mailbox)))
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
