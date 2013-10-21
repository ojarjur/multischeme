;; Copyright 2013 Omar Jarjur
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
(define extended-primitives
  '(exit apply
         call/cc
         call-with-current-continuation
         values
         call-with-values
         dynamic-wind
         with-exception-handler
         make-task
         task?
         task-live?
         task-kill))
(define (escape-symbol symbol)
  (let ((symbol-chars (string->list (symbol->string symbol))))
    (string->symbol (list->string (cons #\_ symbol-chars)))))
(define (escape-symbols expr)
  (cond ((symbol? expr) (escape-symbol expr))
        ((pair? expr)
         (cons (escape-symbols (car expr)) (escape-symbols (cdr expr))))
        (#t expr)))
(define get-next-symbol
  (let ((next-symbol-id 0))
    (lambda ()
      (let ((symbol-name (string-append "s_" (number->string next-symbol-id))))
        (begin
          (set! next-symbol-id (+ next-symbol-id 1))
          (string->symbol symbol-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPS transformation procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-primitive op)
  `(lambda (continuation scheduler done-handler . args)
     (continuation scheduler done-handler (apply ,op args))))
(define (transform-immediate-tail-call expr)
  (lambda (continuation scheduler done-handler)
    `(,continuation ,scheduler ,done-handler ,expr)))
(define (transform-atom-tail-call expr bound-variables)
  (transform-immediate-tail-call
    (if (symbol? expr)
      (if (member expr bound-variables)
        (escape-symbol expr)
        (make-primitive expr))
      expr)))
(define (bind-variables variables bound-variables)
  (cond ((pair? variables)
         (bind-variables
           (car variables)
           (bind-variables (cdr variables) bound-variables)))
        ((symbol? variables) (cons variables bound-variables))
        (#t bound-variables)))
(define (transform-lambda-tail-call params body bound-variables)
  (lambda (continuation scheduler done-handler)
    `(,continuation
      ,scheduler
      ,done-handler
      (lambda (unquote
               (append
                 `(,continuation ,scheduler ,done-handler)
                 (escape-symbols params)))
        (,scheduler
         (lambda (,scheduler ,done-handler)
           ,((transform-tail-call body (bind-variables params bound-variables))
             continuation
             scheduler
             done-handler)))))))
(define (transform-if-tail-call test then else bound-variables)
  (lambda (continuation scheduler done-handler)
    ((transform test bound-variables)
     (lambda (scheduler done-handler test-code)
       `(if ,test-code
          ,((transform-tail-call then bound-variables)
            continuation
            scheduler
            done-handler)
          ,((transform-tail-call else bound-variables)
            continuation
            scheduler
            done-handler)))
     scheduler
     done-handler)))
(define (transform-application-tail-call op args bound-variables)
  (lambda (continuation scheduler done-handler)
    ((transform-args args bound-variables)
     (lambda (scheduler done-handler args-code)
       (if (and (symbol? op) (not (member op bound-variables)))
         `(,continuation ,scheduler ,done-handler ,(cons op args-code))
         ((transform op bound-variables)
          (lambda (scheduler done-handler op-code)
            (append
              `(,op-code ,continuation ,scheduler ,done-handler)
              args-code))
          scheduler
          done-handler)))
     scheduler
     done-handler)))
(define (transform-bindings bindings bound-variables)
  (if (pair? bindings)
    (let ((var (escape-symbols (caar bindings))) (value (cadar bindings)))
      (lambda (builder scheduler done-handler)
        ((transform value bound-variables)
         (lambda (scheduler done-handler value-code)
           ((transform-bindings (cdr bindings) bound-variables)
            (lambda (scheduler done-handler bindings-code)
              (builder
                scheduler
                done-handler
                (cons `(,var ,value-code) bindings-code)))
            scheduler
            done-handler))
         scheduler
         done-handler)))
    (lambda (builder scheduler done-handler)
      (builder scheduler done-handler '()))))
(define (transform-let-tail-call bindings body bound-variables)
  (lambda (continuation scheduler done-handler)
    ((transform-bindings bindings bound-variables)
     (lambda (scheduler done-handler bindings-code)
       `(let ,bindings-code
          ,((transform-tail-call
              body
              (bind-variables (map car bindings) bound-variables))
            continuation
            scheduler
            done-handler)))
     scheduler
     done-handler)))
(define (transform-letrec-tail-call bindings body bound-variables)
  (lambda (continuation scheduler done-handler)
    (let ((bound-variables
            (bind-variables (map car bindings) bound-variables)))
      ((transform-bindings bindings bound-variables)
       (lambda (scheduler done-handler bindings-code)
         `(letrec ,bindings-code
            ,((transform-tail-call body bound-variables)
              continuation
              scheduler
              done-handler)))
       scheduler
       done-handler))))
(define (transform-begin-tail-call statements bound-variables)
  (cond ((not (pair? statements))
         (lambda (continuation scheduler done-handler)
           `(,continuation ,scheduler ,done-handler '())))
        ((not (pair? (cdr statements)))
         (transform-tail-call (car statements) bound-variables))
        (#t
         (lambda (continuation scheduler done-handler)
           ((transform (car statements) bound-variables)
            (lambda (scheduler done-handler statement-code)
              `(begin
                 ,statement-code
                 ,((transform-begin-tail-call (cdr statements) bound-variables)
                   continuation
                   scheduler
                   done-handler)))
            scheduler
            done-handler)))))
(define (transform-set!-tail-call symbol value bound-variables)
  (lambda (continuation scheduler done-handler)
    ((transform value bound-variables)
     (lambda (scheduler done-handler value-code)
       `(,continuation
         ,scheduler
         ,done-handler
         (set! ,(escape-symbol symbol) ,value-code)))
     scheduler
     done-handler)))
(define (transform-tail-call expr bound-variables)
  (cond ((not (pair? expr)) (transform-atom-tail-call expr bound-variables))
        ((eq? (car expr) 'quote) (transform-immediate-tail-call expr))
        ((eq? (car expr) 'if)
         (transform-if-tail-call
           (cadr expr)
           (caddr expr)
           (cadddr expr)
           bound-variables))
        ((eq? (car expr) 'lambda)
         (transform-lambda-tail-call (cadr expr) (caddr expr) bound-variables))
        ((eq? (car expr) 'let)
         (transform-let-tail-call (cadr expr) (caddr expr) bound-variables))
        ((eq? (car expr) 'letrec)
         (transform-letrec-tail-call (cadr expr) (caddr expr) bound-variables))
        ((eq? (car expr) 'begin)
         (transform-begin-tail-call (cdr expr) bound-variables))
        ((eq? (car expr) 'set!)
         (transform-set!-tail-call (cadr expr) (caddr expr) bound-variables))
        (#t
         (transform-application-tail-call
           (car expr)
           (cdr expr)
           bound-variables))))
(define (transform-immediate expr)
  (lambda (builder scheduler done-handler)
    (builder scheduler done-handler expr)))
(define (transform-atom expr bound-variables)
  (transform-immediate
    (if (symbol? expr)
      (if (member expr bound-variables)
        (escape-symbol expr)
        (make-primitive expr))
      expr)))
(define (transform-lambda params body bound-variables)
  (lambda (builder scheduler done-handler)
    (builder
      scheduler
      done-handler
      (let ((continuation (get-next-symbol))
            (scheduler (get-next-symbol))
            (done-handler (get-next-symbol)))
        `(lambda (unquote
                  (append
                    `(,continuation ,scheduler ,done-handler)
                    (escape-symbols params)))
           (,scheduler
            (lambda (,scheduler ,done-handler)
              ,((transform-tail-call
                  body
                  (bind-variables params bound-variables))
                continuation
                scheduler
                done-handler))))))))
(define (transform-if test then else bound-variables)
  (lambda (builder scheduler done-handler)
    ((transform test bound-variables)
     (lambda (scheduler done-handler test-code)
       `(if ,test-code
          ,((transform then bound-variables) builder scheduler done-handler)
          ,((transform else bound-variables) builder scheduler done-handler)))
     scheduler
     done-handler)))
(define (transform-args args bound-variables)
  (if (pair? args)
    (lambda (builder scheduler done-handler)
      ((transform (car args) bound-variables)
       (lambda (scheduler done-handler arg-code)
         ((transform-args (cdr args) bound-variables)
          (lambda (scheduler done-handler args-code)
            (builder scheduler done-handler (cons arg-code args-code)))
          scheduler
          done-handler))
       scheduler
       done-handler))
    (lambda (builder scheduler done-handler)
      (builder scheduler done-handler '()))))
(define (transform-application op args bound-variables)
  (lambda (builder scheduler done-handler)
    ((transform-args args bound-variables)
     (lambda (scheduler done-handler args-code)
       (if (and (symbol? op) (not (member op bound-variables)))
         (builder scheduler done-handler (cons op args-code))
         ((transform op bound-variables)
          (lambda (scheduler done-handler op-code)
            (let ((scheduler2 (get-next-symbol))
                  (done-handler2 (get-next-symbol))
                  (value (get-next-symbol)))
              (append
                `(,op-code
                  (lambda (,scheduler2 ,done-handler2 ,value)
                    ,(builder scheduler2 done-handler2 value))
                  ,scheduler
                  ,done-handler)
                args-code)))
          scheduler
          done-handler)))
     scheduler
     done-handler)))
(define (transform-let bindings body bound-variables)
  (lambda (builder scheduler done-handler)
    (let ((continuation (get-next-symbol)) (value (get-next-symbol)))
      `(let ((,continuation
              (lambda (,scheduler ,done-handler ,value)
                ,(builder scheduler done-handler value))))
         ,((transform-let-tail-call bindings body bound-variables)
           continuation
           scheduler
           done-handler)))))
(define (transform-letrec bindings body bound-variables)
  (lambda (builder scheduler done-handler)
    (let ((continuation (get-next-symbol)) (value (get-next-symbol)))
      `(let ((,continuation
              (lambda (,scheduler ,done-handler ,value)
                ,(builder scheduler done-handler value))))
         ,((transform-letrec-tail-call bindings body bound-variables)
           continuation
           scheduler
           done-handler)))))
(define (transform-begin statements bound-variables)
  (cond ((not (pair? statements))
         (lambda (builder scheduler done-handler)
           (builder scheduler done-handler '())))
        ((not (pair? (cdr statements)))
         (transform (car statements) bound-variables))
        (#t
         (lambda (builder scheduler done-handler)
           ((transform (car statements) bound-variables)
            (lambda (scheduler done-handler statement-code)
              `(begin
                 ,statement-code
                 ,((transform-begin (cdr statements) bound-variables)
                   (lambda (scheduler done-handler statements-code)
                     (builder scheduler done-handler statements-code))
                   scheduler
                   done-handler)))
            scheduler
            done-handler)))))
(define (transform-set! symbol value bound-variables)
  (lambda (builder scheduler done-handler)
    ((transform value bound-variables)
     (lambda (scheduler done-handler value-code)
       (builder
         scheduler
         done-handler
         `(set! ,(escape-symbol symbol) ,value-code)))
     scheduler
     done-handler)))
(define (transform expr bound-variables)
  (cond ((not (pair? expr)) (transform-atom expr bound-variables))
        ((eq? (car expr) 'quote) (transform-immediate expr))
        ((eq? (car expr) 'if)
         (transform-if (cadr expr) (caddr expr) (cadddr expr) bound-variables))
        ((eq? (car expr) 'lambda)
         (transform-lambda (cadr expr) (caddr expr) bound-variables))
        ((eq? (car expr) 'let)
         (transform-let (cadr expr) (caddr expr) bound-variables))
        ((eq? (car expr) 'letrec)
         (transform-letrec (cadr expr) (caddr expr) bound-variables))
        ((eq? (car expr) 'begin) (transform-begin (cdr expr) bound-variables))
        ((eq? (car expr) 'set!)
         (transform-set! (cadr expr) (caddr expr) bound-variables))
        (#t (transform-application (car expr) (cdr expr) bound-variables))))
(define (transform-definition name body defined-globals)
  `(define ,(escape-symbol name)
     ,((transform body defined-globals)
       (lambda (scheduler done-handler value) `(,done-handler ,value))
       'definition-scheduler
       'definition-done-handler)))
(define (transform-statement statement defined-globals)
  (if (and (pair? statement) (eq? (car statement) 'define))
    (transform-definition (cadr statement) (caddr statement) defined-globals)
    `(run-loop
       (make-task-handle
         (lambda (scheduler done-handler)
           ,((transform statement defined-globals)
             (lambda (scheduler done-handler value) `(,done-handler ,value))
             'scheduler
             'done-handler))
         'running
         '()))))
(define multitasking-definitions
  '((define _exit
      (lambda (continuation scheduler done-handler . args)
        (let ((exit-value (if (pair? args) (car args) 0)))
          (done-handler exit-value))))
    (define _apply
      (lambda (continuation scheduler done-handler op args)
        (apply op (append (list continuation scheduler done-handler) args))))
    (define initial-dynamic-context '())
    (define dynamic-context initial-dynamic-context)
    (define (extend-dynamic-context context before after)
      (cons (cons before after) context))
    (define (switch-to-dynamic-context
             context
             continuation
             scheduler
             done-handler)
      (cond ((eq? context dynamic-context)
             (continuation scheduler done-handler))
            ((< (length context) (length dynamic-context))
             ((cdar dynamic-context)
              (lambda (scheduler done-handler . _)
                (begin
                  (set! dynamic-context (cdr dynamic-context))
                  (switch-to-dynamic-context
                    context
                    continuation
                    scheduler
                    done-handler)))
              scheduler
              done-handler))
            (#t
             (switch-to-dynamic-context
               (cdr context)
               (lambda (scheduler done-handler . _)
                 (begin
                   (set! dynamic-context context)
                   ((caar context) continuation scheduler done-handler)))
               scheduler
               done-handler))))
    (define _call-with-current-continuation
      (lambda (continuation scheduler done-handler callback)
        (let ((context dynamic-context))
          (callback
            continuation
            scheduler
            done-handler
            (lambda (dropped scheduler done-handler value)
              (switch-to-dynamic-context
                context
                (lambda (scheduler done-handler . _)
                  (continuation scheduler done-handler value))
                scheduler
                done-handler))))))
    (define _call/cc _call-with-current-continuation)
    (define _values
      (lambda (continuation scheduler done-handler . args)
        (apply continuation (cons scheduler (cons done-handler args)))))
    (define _call-with-values
      (lambda (continuation scheduler done-handler producer consumer)
        (producer
          (lambda (scheduler done-handler . args)
            (apply consumer
                   (append (list continuation scheduler done-handler) args)))
          scheduler
          done-handler)))
    (define _dynamic-wind
      (lambda (continuation scheduler done-handler before thunk after)
        (let ((prior-context dynamic-context)
              (new-context
                (extend-dynamic-context dynamic-context before after)))
          (switch-to-dynamic-context
            new-context
            (lambda (scheduler done-handler . _)
              (thunk (lambda (scheduler done-handler value)
                       (switch-to-dynamic-context
                         prior-context
                         (lambda (scheduler done-handler . _)
                           (continuation scheduler done-handler value))
                         scheduler
                         done-handler))
                     scheduler
                     done-handler))
            scheduler
            done-handler))))
    (define (wrap-exception-handler handler continuation)
      (lambda (k scheduler done-handler)
        (lambda (ex) (k (handler continuation scheduler done-handler ex)))))
    (define (wrap-scheduler-with-handler scheduler wrapped-handler)
      (lambda (callback)
        (scheduler
          (lambda (scheduler done-handler)
            (call/cc
              (lambda (k)
                (with-exception-handler
                  (wrapped-handler k scheduler done-handler)
                  (lambda () (callback scheduler done-handler)))))))))
    (define _with-exception-handler
      (lambda (continuation scheduler done-handler exception-handler thunk)
        (let ((wrapped-handler
                (wrap-exception-handler exception-handler continuation)))
          (call/cc
            (lambda (k)
              (with-exception-handler
                (wrapped-handler k scheduler done-handler)
                (lambda ()
                  (thunk continuation
                         (wrap-scheduler-with-handler
                           scheduler
                           wrapped-handler)
                         done-handler))))))))
    (define (definition-done-handler value) value)
    (define (definition-scheduler callback)
      (call/cc
        (lambda (k)
          (with-exception-handler
            (lambda (x) (k x))
            (lambda ()
              (let ((result
                      (callback
                        (lambda (c) (list 'running c))
                        (lambda (x) (list 'done x)))))
                (if (eq? (car result) 'running)
                  (definition-scheduler (cadr result))
                  (cadr result))))))))
    (define (nested-done-handler value) (list 'done value))
    (define (countdown-scheduler ticks)
      (lambda (callback)
        (if (< ticks 100)
          (callback (countdown-scheduler (+ ticks 1)) nested-done-handler)
          (let ((saved-context dynamic-context))
            (list 'running
                  (lambda (scheduler done-handler)
                    (begin
                      (set! dynamic-context saved-context)
                      (callback scheduler done-handler))))))))
    (define-record-type
      <task-handle>
      (make-task-handle callback state children)
      task-handle?
      (callback task-handle-callback set-task-handle-callback!)
      (state task-handle-state set-task-handle-state!)
      (children task-handle-children set-task-handle-children!))
    (define (thunk-task-handle thunk)
      (make-task-handle
        (lambda (scheduler done-handler)
          (begin
            (set! dynamic-context initial-dynamic-context)
            (thunk (lambda (scheduler done-handler value) (done-handler value))
                   scheduler
                   done-handler)))
        'running
        '()))
    (define (_task? continuation scheduler done-handler expr)
      (continuation scheduler done-handler (task-handle? expr)))
    (define (_task-live? continuation scheduler done-handler expr)
      (continuation
        scheduler
        done-handler
        (and (task-handle? expr) (eq? (task-handle-state expr) 'running))))
    (define (kill-task expr final-state)
      (begin
        (for-each
          (lambda (child) (kill-task child 'killed))
          (task-handle-children expr))
        (set-task-handle-state! expr final-state)))
    (define (_task-kill continuation scheduler done-handler expr)
      (continuation scheduler done-handler (kill-task expr 'killed)))
    (define current-task '())
    (define (_make-task continuation scheduler done-handler thunk)
      (let ((thunk-task (thunk-task-handle thunk)))
        (begin
          (set-task-handle-children!
            current-task
            (cons thunk-task (task-handle-children current-task)))
          (continuation scheduler done-handler thunk-task))))
    (define (progress-task task-handle)
      (begin
        (set! current-task task-handle)
        (call/cc
          (lambda (k)
            (with-exception-handler
              (lambda (error) (k (kill-task task-handle 'failed)))
              (lambda ()
                (let ((result
                        ((task-handle-callback task-handle)
                         (countdown-scheduler 0)
                         nested-done-handler)))
                  (if (eq? (car result) 'running)
                    (set-task-handle-callback! task-handle (cadr result))
                    (kill-task task-handle 'done)))))))))
    (define (run-tasks tasks)
      (if (pair? tasks)
        (let ((task (run-task (car tasks))) (tasks (run-tasks (cdr tasks))))
          (if (eq? (task-handle-state task) 'running) (cons task tasks) tasks))
        '()))
    (define (run-task-children task-handle)
      (set-task-handle-children!
        task-handle
        (run-tasks (task-handle-children task-handle))))
    (define (run-task task-handle)
      (if (eq? (task-handle-state task-handle) 'running)
        (begin
          (run-task-children task-handle)
          (progress-task task-handle)
          task-handle)
        task-handle))
    (define (run-loop root-task)
      (if (eq? (task-handle-state root-task) 'running)
        (run-loop (run-task root-task))))))
