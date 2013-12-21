#!/bin/bash
function run_test() {
    echo 'Testing the support for with-input-from-file'
    echo "
(with-input-from-file
  \"tests/test-with-input-from-file.sh\"
  (lambda ()
    (define (read-contents)
      (let ((first-char (read-char)))
         (if (not (eof-object? first-char))
             (begin (write-char first-char)
                    (read-contents)))))
    (read-contents)))
" | ./multischemec.sh - -o bin/test
    OUTPUT=`bin/test`
    EXPECTED=`cat tests/test-with-input-from-file.sh`
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        return 1
    fi
}
run_test