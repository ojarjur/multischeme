;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example method to encapsulate the publisher-subscriber pattern ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "samples/mailboxes.scm")

(define (with-subscriber subscriber-callback publisher-callback)
  (define (kill-on-drain mailbox task)
    (if (mailbox-empty? mailbox)
        (task-kill task)
        (kill-on-drain mailbox task)))
  (let ((mailbox (make-mailbox 10)))
    (let ((subscriber-task (make-task (lambda ()
                                        (subscriber-callback mailbox)))))
      (begin (publisher-callback mailbox)
             (kill-on-drain mailbox subscriber-task)))))
