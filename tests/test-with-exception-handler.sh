#!/bin/bash
function run_test() {
    echo 'Testing the support for with-exception-handler'
    echo "
(begin (display (call/cc (lambda (k1)
                           (with-exception-handler
                             (lambda (x) (k1 \"Outter\"))
                             (lambda ()
                               (display (call/cc (lambda (k2)
                                                   (with-exception-handler
                                                     (lambda (x) (k2 \"Inner\"))
                                                     (lambda () (cdr 1))))))
                               (newline)
                               (cdr 1))))))
       (newline))
" | ./multischemec.sh - -o bin/test
    OUTPUT=`bin/test`
    EXPECTED=$'Inner\nOutter'
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        return 1
    fi
}
run_test