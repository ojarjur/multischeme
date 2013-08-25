;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample Scheme program that computes the Nth prime number ;;
;; using sieve built from chaining filtering tasks          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for inter-process I/O. For simplicity, ;;
;; we use fixed size, blocking mailboxes.           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-mailbox) (cons #f '()))
(define (send-message mailbox message)
  (if (car mailbox)
      (send-message mailbox message)
      (begin (set-car! mailbox #t)
             (set-cdr! mailbox message)
             mailbox)))
(define (recieve-message mailbox)
  (if (car mailbox)
      (begin (set-car! mailbox #f)
             (cdr mailbox))
      (recieve-message mailbox)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing threads ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define (filter-composites prime mailbox child-mailbox)
  (let ((next-number (recieve-message mailbox)))
    (if (= (remainder next-number prime) 0)
        (filter-composites prime mailbox child-mailbox)
        (begin (send-message child-mailbox next-number)
               (filter-composites prime mailbox child-mailbox)))))
(define (filter-task mailbox primes-mailbox)
  (make-task (lambda ()
               (let ((prime (recieve-message mailbox)))
                 (begin (send-message primes-mailbox prime)
                        (let ((child-mailbox (make-mailbox))
                              (child (filter-task child-mailbox
                                                  primes-mailbox)))
                          (filter-composites prime mailbox
                                             child-mailbox)))))))
(define (drive next-number mailbox)
  (begin (send-message mailbox next-number)
         (drive (+ next-number 1) mailbox)))
(define (driver-task mailbox)
  (make-task (lambda () (drive 2 mailbox))))
(define (read-primes count primes-mailbox)
  (if (> count 0)
      (cons (recieve-message primes-mailbox)
            (read-primes (- count 1) primes-mailbox))
      '()))
(let ((numbers-mailbox (make-mailbox))
      (primes-mailbox (make-mailbox))
      (filter (filter-task numbers-mailbox primes-mailbox))
      (driver (driver-task numbers-mailbox)))
  (begin (display (read-primes 100 primes-mailbox))
         (newline)
         (task-kill filter)
         (task-kill driver)))
