#!/bin/bash
function run_test() {
    echo 'Testing the support for task state handling.'
    echo "
(define (wait-for task) (if (task-live? task) (wait-for task)))
(let* ((infinite-loop (lambda () ((lambda (x) (x x)) (lambda (x) (x x)))))
       (running-task (make-task infinite-loop))
       (done-task (make-task (lambda () (begin))))
       (killed-task (make-task infinite-loop)))
  (begin (wait-for done-task)
         (task-kill killed-task)
         (if (and (task? killed-task)
                  (task? done-task)
                  (task? running-task)
                  (task-killed? killed-task)
                  (task-done? done-task)
                  (task-live? running-task))
             (display 'Yes)
             (display 'No))))
(newline)" | ./multischemec.sh - -o bin/test
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