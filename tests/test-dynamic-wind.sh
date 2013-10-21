#!/bin/bash
function run_test() {
    echo 'Testing the support for dynamic-wind'
    echo "
(define cont '())
(dynamic-wind
  (lambda ()
    (display 'Before))
  (lambda ()
    (display (call/cc
               (lambda (k)
                 (begin (set! cont k)
                        (k 1))))))
  (lambda ()
    (display 'After)))
(cont 2)
(newline)" | ./multischemec.sh -o bin/test
    OUTPUT=`bin/test`
    EXPECTED="Before1AfterBefore2After"
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        return 1
    fi
}
run_test