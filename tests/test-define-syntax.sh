#!/bin/bash
function run_test() {
    echo 'Testing the support for define-syntax'
    ./multischemec.sh -o bin/test <(echo '(load "samples/mailboxes.scm")
(define-syntax mailbox-display
  (syntax-rules ()
    ((mailbox-display mailbox)
     (begin (display `((empty? ,(mailbox-empty? mailbox))
                       (full? ,(mailbox-full? mailbox))))
            (newline)))))
(let ((mailbox (make-mailbox 2)))
  (begin (mailbox-display mailbox)
         (mailbox-send mailbox 1)
         (mailbox-display mailbox)
         (mailbox-send mailbox 2)
         (mailbox-display mailbox)
         (display (mailbox-receive mailbox))
         (newline)
         (mailbox-display mailbox)
         (mailbox-send mailbox 3)
         (mailbox-display mailbox)
         (display (mailbox-receive mailbox))
         (newline)
         (display (mailbox-receive mailbox))
         (newline)))')
    OUTPUT=`bin/test`
    EXPECTED="((empty? #t) (full? #f))
((empty? #f) (full? #f))
((empty? #f) (full? #t))
1
((empty? #f) (full? #f))
((empty? #f) (full? #t))
2
3"
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        echo "$OUTPUT"
        return 1
    fi
}
run_test