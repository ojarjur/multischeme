#!/bin/bash
function run_test() {
    echo 'Testing the sample code for blocking mailboxes'
    ./multischemec.sh -o bin/test <(echo '(load "samples/mailboxes.scm")
(let ((mailbox (make-mailbox 2)))
  (begin (display (mailbox-empty? mailbox))
         (newline)
         (mailbox-send mailbox 1)
         (display (mailbox-empty? mailbox))
         (newline)
         (display (mailbox-full? mailbox))
         (newline)
         (mailbox-send mailbox 2)
         (display (mailbox-full? mailbox))
         (newline)
         (display (mailbox-receive mailbox))
         (newline)
         (display (mailbox-full? mailbox))
         (newline)
         (mailbox-send mailbox 3)
         (display (mailbox-full? mailbox))
         (newline)
         (display (mailbox-receive mailbox))
         (newline)
         (display (mailbox-receive mailbox))
         (newline)))')
    OUTPUT=`bin/test`
    EXPECTED="#t
#f
#f
#t
1
#f
#t
2
3"
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        return 1
    fi
}
run_test