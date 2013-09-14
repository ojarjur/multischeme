#!/bin/bash
function run_test() {
    echo 'Testing the support for with-exception-handler'
    ./multischemec.sh -o bin/test <(echo "(begin (display (call/cc (lambda (k) (with-exception-handler (lambda (x) (k \"Yes\")) (lambda () (cdr 1)))))) (newline))")
    OUTPUT=`bin/test`
    EXPECTED="Yes"
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        return 1
    fi
}
run_test