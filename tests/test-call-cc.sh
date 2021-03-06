#!/bin/bash
function run_test() {
    echo 'Testing the support for call/cc'
    ./multischemec.sh -o bin/test <(echo "(begin (display (call/cc (lambda (k) (begin (k \"Yes\") \"No\")))) (newline))")
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