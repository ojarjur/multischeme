#!/bin/bash
function run_test() {
    echo 'Testing the support for with-output-to-file'
    echo "
(with-output-to-file
  \"bin/test-output\"
  (lambda ()
    (define (read-contents)
      (let ((first-char (read-char)))
         (if (not (eof-object? first-char))
             (begin (write-char first-char)
                    (read-contents)))))
    (read-contents)))
" | ./multischemec.sh - -o bin/test
bin/test < tests/test-with-output-to-file.sh
    OUTPUT=`cat bin/test-output`
    EXPECTED=`cat tests/test-with-output-to-file.sh`
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        return 1
    fi
}
run_test