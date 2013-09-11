#!/bin/bash
function run_test() {
    echo 'Testing bootstrapping of the transformation'
    ./bootstrap.sh
    bin/multischemec < src/main.scm | csc - -w -o bin/multischemec2
    OUTPUT=`diff <(bin/multischemec < src/main.scm) <(bin/multischemec2 < src/main.scm)`
    if [ -z "$OUTPUT" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        return 1
    fi
}
run_test