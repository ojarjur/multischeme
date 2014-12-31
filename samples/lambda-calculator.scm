;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample Scheme program that implements a multitasking REPL ;;
;; for the untyped Lambda Calculus.                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "samples/mailboxes.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We need to define two sets of types, one for AST nodes, and   ;;
;; one for evaluated expressions. The difference between the two ;;
;; sets is in the way lambda expressions are represented. In the ;;
;; AST type a lambda expression is just a variable and body,     ;;
;; while an evaluated lambda expression also has an environment. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In both sets, we use symbols for variables, and pairs for     ;;
;; lambda-calculus applications. That leaves lambda expressions, ;;
;; for which we define two new record types.                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type <lambda-ast-node>
  (make-lambda-ast-node variable body)
  lambda-ast-node?
  (variable lambda-ast-node-variable)
  (body lambda-ast-node-body))
(define-record-type <lambda-expr>
  (make-lambda-expr variable body env)
  lambda-expr?
  (variable lambda-expr-variable)
  (body lambda-expr-body)
  (env lambda-expr-env))

(define (lc-eval expr)
  (define (empty-env variable) variable)
  (define (bind symbol value env)
    (lambda (variable)
      (if (eq? variable symbol) value (env variable))))
  (define (apply op arg)
    (if (lambda-expr? op)
        (let ((variable (lambda-expr-variable op))
              (body (lambda-expr-body op))
              (env (lambda-expr-env op)))
          (eval body (bind variable arg env)))
        (cons op arg)))
  (define (eval expr env)
    (cond ((symbol? expr) (env expr))
          ((pair? expr)
           (apply (eval (car expr) env)
                  (eval (cdr expr) env)))
          ((lambda-ast-node? expr)
           (let ((variable (lambda-ast-node-variable expr))
                 (body (lambda-ast-node-body expr)))
             (make-lambda-expr variable body env)))
          (#t expr)))
  (eval expr empty-env))

(define (lc-display expr)
  (define (recursive-display head? tail? expr)
    (cond ((pair? expr)
           (if (not head?)
               (begin (display #\()
                      (recursive-display #t #f (car expr))
                      (display #\space)
                      (recursive-display #f #t (cdr expr))
                      (display #\)))
               (begin (recursive-display head? #f (car expr))
                      (display #\space)
                      (recursive-display #f tail? (cdr expr)))))
          ((lambda-expr? expr)
           (let ((variable (lambda-expr-variable expr))
                 (body (lambda-expr-body expr)))
             (if (not tail?)
                 (begin (display #\()
                        (display #\\)
                        (display variable)
                        (display #\.)
                        (recursive-display #t #t body)
                        (display #\)))
                 (begin (display #\\)
                        (display variable)
                        (display #\.)
                        (recursive-display #t #t body)))))
          (#t (display expr))))
  (recursive-display #t #t expr))

(define (lc-read)
  (define (skip-rest-of-line)
    (do ((next-char (peek-char) (peek-char)))
        ((or (eof-object? next-char)
             (eq? next-char #\newline)))
      (read-char)))
  (define (skip-whitespace terminating-character)
    (do ((next-char (peek-char) (peek-char)))
        ((or (eq? next-char terminating-character)
             (not (char-whitespace? next-char))))
      (read-char)))
  (define (expect char error-cont)
    (skip-whitespace char)
    (let ((next-char (peek-char)))
      (cond ((eof-object? next-char) (error-cont next-char))
            ((eq? next-char char) (read-char))
            (#t (begin (skip-rest-of-line)
                       (error-cont (string-append "Expected '"
                                                  (list->string `(,char))
                                                  "', but saw '"
                                                  (list->string `(,next-char))
                                                  "'.")))))))
  (define (read-symbol reversed-chars)
    (let ((next-char (peek-char)))
      (if (or (eof-object? next-char)
              (char-whitespace? next-char)
              (eq? next-char #\\)
              (eq? next-char #\.)
              (eq? next-char #\))
              (eq? next-char #\())
          (string->symbol (list->string (reverse reversed-chars)))
          (read-symbol (cons (read-char) reversed-chars)))))
  (define (read-args op terminating-character error-cont)
    (skip-whitespace terminating-character)
    (let ((next-char (peek-char)))
      (cond ((eof-object? next-char) (error-cont next-char))
            ((eq? next-char terminating-character) op)
            (#t (read-args (cons op (read-element terminating-character
                                                  error-cont))
                           terminating-character
                           error-cont)))))
  (define (read-list terminating-character error-cont)
    (read-args (read-element terminating-character error-cont)
               terminating-character
               error-cont))
  (define (read-element terminating-character error-cont)
    (skip-whitespace terminating-character)
    (let ((next-char (peek-char)))
      (cond ((eof-object? next-char) (error-cont next-char))
            ((or (eq? next-char #\.) (eq? next-char #\)))
             (begin (skip-rest-of-line)
                    (error-cont (string-append "Unexpected character '"
                                               (list->string `(,next-char))
                                               "'"))))
            ((eq? next-char #\()
             (begin (read-char)
                    (let ((expr (read-list #\) error-cont)))
                      (begin (expect #\) error-cont)
                             expr))))
            ((eq? next-char #\\)
             (begin (read-char)
                    (skip-whitespace terminating-character)
                    (let ((variable (read-symbol '())))
                      (begin (skip-whitespace terminating-character)
                             (expect #\. error-cont)
                             (let ((body (read-list terminating-character
                                                    error-cont)))
                               (make-lambda-ast-node variable body))))))
            (#t (read-symbol '())))))
  (call/cc (lambda (cont)
             (with-exception-handler
              (lambda (ex)
                (display ex)
                (cont "Internal error"))
              (lambda ()
                (let ((expr (read-list #\newline cont)))
                  (begin (expect #\newline cont)
                         expr)))))))

;; TODO: Remove this next definition after R7RS-small is ubiquitous.
(define flush-output-port
  (call/cc (lambda (k)
             (with-exception-handler
              (lambda (ex)
                (k flush-output))
              (lambda ()
                (flush-output-port)
                flush-output-port)))))

(define results-mailbox (make-mailbox 100))
(define (display-results)
  (if (not (mailbox-empty? results-mailbox))
      (let ((result (mailbox-receive results-mailbox)))
        (begin (display (car result))
               (display ": ")
               (lc-display (cdr result))
               (newline)
               (display-results)))))
(define (make-eval-task id expr)
  (make-task (lambda ()
               (mailbox-send results-mailbox
                             (cons id (lc-eval expr))))))
(define (repl next-id)
  (begin (display-results)
         (display next-id)
         (display "> ")
         (flush-output-port)
         (let ((expr (lc-read)))
           (cond ((eof-object? expr) (exit))
                 ((and (symbol? expr) (null? (string->list (symbol->string expr))))
                  (repl next-id))
                 ((string? expr)
                  (begin (display expr)
                         (repl (+ next-id 1))))
                 (#t (begin (make-eval-task next-id expr)
                            (repl (+ next-id 1))))))))

(define usage-string
  (string-append "Multitasking Lambda-Calculus Interpreter.\n"
                 "Use '\\' for lambda, and type <CTRL+D> to quit.\n"))
(begin (display usage-string)
       (repl 0))
