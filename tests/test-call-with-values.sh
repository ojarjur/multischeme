#!/bin/bash
function run_test() {
    echo 'Testing the support for call-with-values'
    echo "(call-with-values (lambda () (values 4 5 6)) (lambda (a b c) (display (+ (* a b) c)))) (newline)" | ./multischemec.sh - -o bin/test
    OUTPUT=`bin/test`
    EXPECTED="26"
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        return 1
    fi
}
run_test