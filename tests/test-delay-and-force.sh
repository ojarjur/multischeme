#!/bin/bash
function run_test() {
    echo 'Testing the support for delay and force'
    echo "
(define infinite-loop (delay ((lambda (x) (x x)) (lambda (x) (x x)))))
(define simple-promise (delay (+ 1 2)))
(display (if (and (promise? infinite-loop)
                  (promise? simple-promise)
                  (not (promise? (force simple-promise))))
             \"Yes\"
             \"No\"))
(display (force simple-promise))
(newline)" | ./multischemec.sh -o bin/test
    OUTPUT=`bin/test`
    EXPECTED="Yes3"
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        return 1
    fi
}
run_test