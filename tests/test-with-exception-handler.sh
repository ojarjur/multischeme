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
                                                     (lambda (x) (k2 \"Inner1\"))
                                                     (lambda () (cdr 1))))))
                               (newline)
                               (display (call/cc (lambda (k3)
                                                   (with-exception-handler
                                                     (lambda (x) (k3 \"Inner2\"))
                                                     (lambda () (raise 1))))))
                               (newline)
                               (cdr 1))))))
       (newline))
(begin (display (call/cc (lambda (k)
                           (with-exception-handler
                             (lambda (ex) (k \"... and unhandled\"))
                             (lambda ()
                               (with-exception-handler
                                 (lambda (ex) (display \"Caught\"))
                                 (lambda () (raise \"Error\"))))))))
       (newline))
(let ((task1 (make-task
               (lambda ()
                 (call/cc
                   (lambda (k)
                     (with-exception-handler
                       (lambda (ex) (begin (display \"Task1\")
                                           (newline)
                                           (k 1)))
                       (lambda () (cdr 1))))))))
      (task2 (make-task
               (lambda () (raise \"Task2\")))))
  (define (wait)
    (if (or (task-live? task1) (task-live? task2))
        (wait)))
  (wait))
(raise 0)
" | ./multischemec.sh - -o bin/test
    OUTPUT=`bin/test`
    EXPECTED=$'Inner1\nInner2\nOutter\nCaught... and unhandled\nTask1'
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo $'\tPassed'
        return 0
    else
        echo $'\tFailed'
        return 1
    fi
}
run_test