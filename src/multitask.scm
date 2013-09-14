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

(define supported-primitives
  '(eq? equal? cons null? pair? symbol? procedure?
        string? number? char? boolean? port? eof-object?
        + - * / quotient remainder < > = member list append not
        make-string string-length string-append number->string
        symbol->string string->symbol list->string string->list
        char->integer integer->char
        read read-char display write print newline current-input-port
        vector? make-vector vector-ref vector-set!
        set-car! set-cdr! car cdr
        caar cadr cdar cddr
        caaar caadr cadar caddr
        cdaar cdadr cddar cdddr
        caaaar caaadr caadar caaddr
        cadaar cadadr caddar cadddr
        cdaaar cdaadr cdadar cdaddr
        cddaar cddadr cdddar cddddr
        open-output-file open-input-file
        close-output-port close-input-port))
(define extended-primitives
  '(exit call/cc call-with-current-continuation with-exception-handler
         map for-each call-with-input-file call-with-output-file
         make-task task? task-live? task-kill))
(define (escape-symbol symbol)
  (let ((symbol-chars (string->list (symbol->string symbol))))
    (if (or (eq? (car symbol-chars) #\_)
            (member symbol supported-primitives)
            (member symbol extended-primitives))
        (string->symbol (list->string (cons #\_ symbol-chars)))
        symbol)))
(define (escape-symbols expr)
  (cond ((symbol? expr) (escape-symbol expr))
        ((pair? expr)
         (cons (escape-symbols (car expr)) (escape-symbols (cdr expr))))
        (#t expr)))
(define get-next-symbol
  (let ((next-symbol-id 0))
    (lambda ()
      (let ((symbol-name (string-append "_" (number->string next-symbol-id))))
        (begin (set! next-symbol-id (+ next-symbol-id 1))
               (string->symbol symbol-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting to indicate whether or not overloading of primitive procedures is ;;
;; supported. This is required in order to be fully compliant with the       ;;
;; Scheme language, but most programs will not need it (as they won't        ;;
;; redefine any primitive procedures), and it adds a significant performance ;;
;; overhead.                                                                 ;;
;;                                                                           ;;
;; In some testing, the multitasking rewrite without support for overloading ;;
;; primitives adds a 10% performance penalty, over single tasking code, but  ;;
;; with overloading support, it adds a 50% performance penalty.              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define overload-primitives? #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CPS transformation procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (transform-immediate-tail-call expr)
  (lambda (continuation scheduler done-handler)
    `(,continuation ,scheduler ,done-handler ,expr)))
(define (transform-atom-tail-call expr)
  (transform-immediate-tail-call (if (symbol? expr) (escape-symbol expr) expr)))
(define (transform-lambda-tail-call params body)
  (lambda (continuation scheduler done-handler)
    `(,continuation ,scheduler
                    ,done-handler
                    (lambda ,(append `(,continuation ,scheduler ,done-handler)
                                     (escape-symbols params))
                      (,scheduler (lambda (,scheduler ,done-handler)
                                    ,((transform-tail-call body)
                                      continuation
                                      scheduler
                                      done-handler)))))))
(define (transform-if-tail-call test then else)
  (lambda (continuation scheduler done-handler)
    ((transform test)
     (lambda (scheduler done-handler test-code)
       `(if ,test-code
            ,((transform-tail-call then) continuation scheduler done-handler)
            ,((transform-tail-call else) continuation scheduler done-handler)))
     scheduler
     done-handler)))
(define (transform-application-tail-call op args)
  (lambda (continuation scheduler done-handler)
    ((transform-args args)
     (lambda (scheduler done-handler args-code)
       (if (or overload-primitives?
	       (not (member op supported-primitives)))
	   ((transform op)
	    (lambda (scheduler done-handler op-code)
	      (append `(,op-code
			,continuation
			,scheduler
			,done-handler)
		      args-code))
	    scheduler
	    done-handler)
	   `(,continuation
	     ,scheduler
	     ,done-handler
	     ,(cons op args-code))))
     scheduler
     done-handler)))
(define (transform-let-tail-call bindings body)
  (lambda (continuation scheduler done-handler)
    ((transform-bindings bindings)
     (lambda (scheduler done-handler bindings-code)
       `(let ,bindings-code
          ,((transform-tail-call body)
            continuation
            scheduler
            done-handler)))
     scheduler
     done-handler)))
(define (transform-letrec-tail-call bindings body)
  (lambda (continuation scheduler done-handler)
    ((transform-bindings bindings)
     (lambda (scheduler done-handler bindings-code)
       `(letrec ,bindings-code
          ,((transform-tail-call body)
            continuation
            scheduler
            done-handler)))
     scheduler
     done-handler)))
(define (transform-begin-tail-call statements)
  (cond ((not (pair? statements))
         (lambda (continuation scheduler done-handler)
           `(,continuation ,scheduler ,done-handler '())))
        ((not (pair? (cdr statements)))
         (transform-tail-call (car statements)))
        (#t (lambda (continuation scheduler done-handler)
              ((transform (car statements))
               (lambda (scheduler done-handler statement-code)
                 `(begin ,statement-code
                         ,((transform-begin-tail-call (cdr statements))
                           continuation
                           scheduler
                           done-handler)))
               scheduler
               done-handler)))))
(define (transform-set!-tail-call symbol value)
  (lambda (continuation scheduler done-handler)
    ((transform value)
     (lambda (scheduler done-handler value-code)
       `(,continuation ,scheduler ,done-handler
                       (set! ,(escape-symbol symbol) ,value-code)))
     scheduler
     done-handler)))
(define (transform-tail-call expr)
  (cond ((not (pair? expr)) (transform-atom-tail-call expr))
        ((eq? (car expr) 'quote) (transform-immediate-tail-call expr))
        ((eq? (car expr) 'if)
         (transform-if-tail-call (cadr expr) (caddr expr) (cadddr expr)))
        ((eq? (car expr) 'lambda)
         (transform-lambda-tail-call (cadr expr) (caddr expr)))
        ((eq? (car expr) 'let)
         (transform-let-tail-call (cadr expr) (caddr expr)))
        ((eq? (car expr) 'letrec)
         (transform-letrec-tail-call (cadr expr) (caddr expr)))
        ((eq? (car expr) 'begin)
         (transform-begin-tail-call (cdr expr)))
        ((eq? (car expr) 'set!)
         (transform-set!-tail-call (cadr expr) (caddr expr)))
        (#t (transform-application-tail-call (car expr) (cdr expr)))))

(define (transform-immediate expr)
  (lambda (builder scheduler done-handler)
    (builder scheduler done-handler expr)))
(define (transform-atom expr)
  (transform-immediate (if (symbol? expr) (escape-symbol expr) expr)))
(define (transform-lambda params body)
  (lambda (builder scheduler done-handler)
    (builder scheduler
             done-handler
             (let ((continuation (get-next-symbol))
                   (scheduler (get-next-symbol))
                   (done-handler (get-next-symbol)))
               `(lambda ,(append `(,continuation ,scheduler ,done-handler)
                                 (escape-symbols params))
                  (,scheduler (lambda (,scheduler ,done-handler)
                                ,((transform-tail-call body)
                                  continuation
                                  scheduler
                                  done-handler))))))))
(define (transform-if test then else)
  (lambda (builder scheduler done-handler)
    ((transform test)
     (lambda (scheduler done-handler test-code)
       `(if ,test-code
            ,((transform then) builder scheduler done-handler)
            ,((transform else) builder scheduler done-handler)))
     scheduler
     done-handler)))
(define (transform-args args)
  (if (pair? args)
      (lambda (builder scheduler done-handler)
        ((transform (car args))
         (lambda (scheduler done-handler arg-code)
           ((transform-args (cdr args))
            (lambda (scheduler done-handler args-code)
              (builder scheduler
                       done-handler
                       (cons arg-code args-code)))
            scheduler
            done-handler))
         scheduler
         done-handler))
      (lambda (builder scheduler done-handler)
        (builder scheduler done-handler '()))))
(define (transform-application op args)
  (lambda (builder scheduler done-handler)
    ((transform-args args)
     (lambda (scheduler done-handler args-code)
       (if (or overload-primitives?
	       (not (member op supported-primitives)))
	   ((transform op)
	    (lambda (scheduler done-handler op-code)
	      (let ((scheduler2 (get-next-symbol))
		    (done-handler2 (get-next-symbol))
		    (value (get-next-symbol)))
		(append `(,op-code (lambda (,scheduler2 ,done-handler2 ,value)
				     ,(builder scheduler2 done-handler2 value))
				   ,scheduler
				   ,done-handler)
			args-code)))
	    scheduler
	    done-handler)
	   (builder scheduler done-handler (cons op args-code))))
     scheduler
     done-handler)))
(define (transform-bindings bindings)
  (if (pair? bindings)
      (let ((var (caar bindings))
            (value (cadar bindings)))
        (lambda (builder scheduler done-handler)
          ((transform value)
           (lambda (scheduler done-handler value-code)
             ((transform-bindings (cdr bindings))
              (lambda (scheduler done-handler bindings-code)
                (builder scheduler done-handler
                         (cons `(,var ,value-code) bindings-code)))
              scheduler
              done-handler))
           scheduler
           done-handler)))
      (lambda (builder scheduler done-handler)
        (builder scheduler done-handler '()))))
(define (transform-let bindings body)
  (lambda (builder scheduler done-handler)
    (let ((continuation (get-next-symbol))
          (value (get-next-symbol)))
      `(let ((,continuation (lambda (,scheduler ,done-handler ,value)
                              ,(builder scheduler done-handler value))))
         ,((transform-let-tail-call bindings body)
           continuation
           scheduler
           done-handler)))))
(define (transform-letrec bindings body)
  (lambda (builder scheduler done-handler)
    (let ((continuation (get-next-symbol))
          (value (get-next-symbol)))
      `(let ((,continuation (lambda (,scheduler ,done-handler ,value)
                              ,(builder scheduler done-handler value))))
         ,((transform-letrec-tail-call bindings body)
           continuation
           scheduler
           done-handler)))))
(define (transform-begin statements)
  (cond ((not (pair? statements))
         (lambda (builder scheduler done-handler)
           (builder scheduler done-handler '())))
        ((not (pair? (cdr statements)))
         (transform (car statements)))
        (#t (lambda (builder scheduler done-handler)
              ((transform (car statements))
               (lambda (scheduler done-handler statement-code)
                 `(begin ,statement-code
                         ,((transform-begin (cdr statements))
                           (lambda (scheduler done-handler statements-code)
                             (builder scheduler
                                      done-handler
                                      statements-code))
                           scheduler
                           done-handler)))
               scheduler
               done-handler)))))
(define (transform-set! symbol value)
  (lambda (builder scheduler done-handler)
    ((transform value)
     (lambda (scheduler done-handler value-code)
       (builder scheduler
                done-handler
                `(set! ,(escape-symbol symbol) ,value-code)))
     scheduler
     done-handler)))
(define (transform expr)
  (cond ((not (pair? expr)) (transform-atom expr))
        ((eq? (car expr) 'quote) (transform-immediate expr))
        ((eq? (car expr) 'if)
         (transform-if (cadr expr) (caddr expr) (cadddr expr)))
        ((eq? (car expr) 'lambda)
         (transform-lambda (cadr expr) (caddr expr)))
        ((eq? (car expr) 'let)
         (transform-let (cadr expr) (caddr expr)))
        ((eq? (car expr) 'letrec)
         (transform-letrec (cadr expr) (caddr expr)))
        ((eq? (car expr) 'begin)
         (transform-begin (cdr expr)))
        ((eq? (car expr) 'set!)
         (transform-set! (cadr expr) (caddr expr)))
        (#t (transform-application (car expr) (cdr expr)))))
(define (transform-definition name body)
  `(define ,(escape-symbol name)
     ,((transform (rewrite body))
       (lambda (scheduler done-handler value)
         `(,done-handler ,value))
       '_definition-scheduler
       '_definition-done-handler)))
(define (transform-statement statement)
  (if (and (pair? statement)
           (eq? (car statement) 'define))
      (transform-definition (cadr statement)
                            (caddr statement))
      `(_run-loop (_make-task-handle
                   (lambda (scheduler done-handler)
                     ,((transform statement)
                       (lambda (scheduler done-handler value)
                         `(,done-handler ,value))
                       'scheduler
                       'done-handler))
                   'running
                   '()))))

(define (make-primitive op)
  `(define ,(escape-symbol op)
     (lambda (continuation scheduler done-handler . args)
       (let ((value (_apply ,op args)))
         (continuation scheduler done-handler value)))))
(define primitives
  (append '((define _apply apply))
          (map make-primitive supported-primitives)))

(define multitasking-definitions
  '((define _exit
      (lambda (continuation scheduler done-handler . args)
	(let ((exit-value (if (pair? args) (car args) 0)))
	  (done-handler exit-value))))
    (define _call-with-current-continuation
      (lambda (continuation scheduler done-handler callback)
	(callback continuation
		  scheduler
		  done-handler
		  (lambda (dropped scheduler done-handler value)
		    (continuation scheduler done-handler value)))))
    (define _call/cc _call-with-current-continuation)
    (define (_wrap-exception-handler handler continuation)
      (lambda (k scheduler done-handler)
        (lambda (ex)
          (k (handler continuation scheduler done-handler ex)))))
    (define (_wrap-scheduler-with-handler scheduler wrapped-handler)
      (lambda (callback)
        (scheduler (lambda (scheduler done-handler)
                     (call/cc (lambda (k)
                                (with-exception-handler
                                 (wrapped-handler k scheduler done-handler)
                                 (lambda () (callback scheduler
                                                      done-handler)))))))))
    (define _with-exception-handler
      (lambda (continuation scheduler done-handler exception-handler thunk)
        (let ((wrapped-handler (_wrap-exception-handler exception-handler
                                                        continuation)))
          (call/cc (lambda (k)
                     (with-exception-handler
                      (wrapped-handler k scheduler done-handler)
                      (lambda ()
                        (thunk continuation
                               (_wrap-scheduler-with-handler scheduler
                                                             wrapped-handler)
                               done-handler))))))))

    (define (_definition-done-handler value) value)
    (define (_definition-scheduler callback)
      (call/cc (lambda (k)
                 (with-exception-handler
                  (lambda (x) (k x))
                  (lambda ()
                    (let ((result (callback (lambda (c) (list 'running c))
                                            (lambda (x) (list 'done x)))))
                      (if (eq? (car result) 'running)
                          (_definition-scheduler (cadr result))
                          (cadr result))))))))

    (define (_nested-done-handler value) (list 'done value))
    (define (_countdown-scheduler ticks)
      (lambda (callback)
        (if (< ticks 100)
            (callback (_countdown-scheduler (+ ticks 1))
                      _nested-done-handler)
            (list 'running callback))))

    (define-record-type <_task-handle>
      (_make-task-handle callback state children)
      _task-handle?
      (callback _task-handle-callback _set-task-handle-callback!)
      (state _task-handle-state _set-task-handle-state!)
      (children _task-handle-children _set-task-handle-children!))
    (define (_thunk-task-handle thunk)
      (_make-task-handle (lambda (scheduler done-handler)
                           (thunk (lambda (scheduler done-handler value)
                                    (done-handler value))
                                  scheduler
                                  done-handler))
                         'running
                         '()))
    (define (_task? continuation scheduler done-handler expr)
      (continuation scheduler done-handler (_task-handle? expr)))
    (define (_task-live? continuation scheduler done-handler expr)
      (continuation scheduler done-handler
                    (and (_task-handle? expr)
                         (eq? (_task-handle-state expr) 'running))))
    (define (_kill-task expr final-state)
      (begin (for-each (lambda (child) (_kill-task child 'killed))
                       (_task-handle-children expr))
             (_set-task-handle-state! expr final-state)))
    (define (_task-kill continuation scheduler done-handler expr)
      (continuation scheduler done-handler (_kill-task expr 'killed)))

    (define _current-task '())
    (define (_make-task continuation scheduler done-handler thunk)
      (let ((thunk-task (_thunk-task-handle thunk)))
        (begin (_set-task-handle-children!
                _current-task
                (cons thunk-task
                      (_task-handle-children _current-task)))
               (continuation scheduler done-handler thunk-task))))
    (define (_progress-task task-handle)
      (begin (set! _current-task task-handle)
             (call/cc (lambda (k)
                        (with-exception-handler
                         (lambda (error)
                           (k (_kill-task task-handle 'failed)))
                         (lambda ()
                           (let ((result ((_task-handle-callback task-handle)
                                          (_countdown-scheduler 0)
                                          _nested-done-handler)))
                             (if (eq? (car result) 'running)
                                 (_set-task-handle-callback! task-handle
                                                             (cadr result))
                                 (_kill-task task-handle 'done)))))))))
    (define (_run-tasks tasks)
      (if (pair? tasks)
          (let ((task (_run-task (car tasks)))
                (tasks (_run-tasks (cdr tasks))))
            (if (eq? (_task-handle-state task) 'running)
                (cons task tasks)
                tasks))
          '()))
    (define (_run-task-children task-handle)
      (_set-task-handle-children!
       task-handle
       (_run-tasks (_task-handle-children task-handle))))
    (define (_run-task task-handle)
      (if (eq? (_task-handle-state task-handle) 'running)
          (begin (_run-task-children task-handle)
                 (_progress-task task-handle)
                 task-handle)
          task-handle))
    (define (_run-loop root-task)
      (if (eq? (_task-handle-state root-task) 'running)
          (_run-loop (_run-task root-task))))))
