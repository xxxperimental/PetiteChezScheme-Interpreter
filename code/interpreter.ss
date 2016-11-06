;; top-level-eval evaluates a form in the global environment
(define (top-level-eval form) (eval-exp-cps form (empty-env) (id-k (lambda (x) x))))

(define procmap-cps
  (lambda (cond ls maptype k)
    (if (null? ls)
        (k #t)
        (map-cps cond ls (procm-k maptype k)))))

(define (ordered-ormap-cps list env k)
  (if (null? list)
      (apply-k k #f)
      (eval-exp-cps (car list) env (ormap-k (cdr list) env k))))

(define (ordered-anmap-cps list env)
  (if (null? list)
      (apply-k k #t)
      (eval-exp-cps (car list) env (anmap-k (cdr list) env k))))

(define (eval-exp-helper-cps ex env k)
  (if (and (list? ex) (not (null? ex)) (or (equal? (car ex) 'lit-exp) (equal? (car ex) 'var-exp) (equal? (car ex) 'app-exp)
                                           (equal? (car ex) 'if2-exp) (equal? (car ex) 'let-exp) (equal? (car ex) 'lambda-exp)))
      (eval-exp-cps ex env k)
      (apply-k k ex)))

(define (apply-k kont v)
  (cases cont kont
         [id-k    (x)
                  (x v)]
         [var-k   (env k)
                  (eval-exp-helper-cps v env k)]
         [app1-k  (args env k)
                  (apply-proc-cps v args env k)]
         [app2-k  (args env k)
                  (map-cps (lambda (x k) (eval-exp-cps x env k)) args (app2-kk v env k))]
         [app2-kk (vv env k)
                  (apply-proc-cps vv v env k)]
         [rator-k (rands env k)
                  (eval-rands rands env (rands-k val k))]
         [rands-k (rator-val k)
                  (apply-proc-cps rator-val val k)]
         [if1-k   (then-exp env k)
                  (if v (eval-exp-cps then-exp env k) (void))]
         [if2-k   (then-exp else-exp env k)
                  (if v (eval-exp-cps then-exp env k) (eval-exp-cps else-exp env k))]
         [let-k   (vars bodies env k)
                  (eval-bodies bodies (extend-env vars val env) k)]
         [ormap-k (lst env k)
                  (or v (ordered-ormap-cps lst env k))]
         [anmap-k (lst env k)
                  (or v (ordered-anmap-cps lst env k))]
         [procm-k (maptype k)
                  (apply-k k (apply maptype v))]
         [evbod-k (bodies env k)
                  (eval-bodies-cps bodies env k)]))

(define (eval-exp-cps exp env k)
  (cases expression exp
         [lit-exp     (datum) (apply-k k datum)]
         [litq-exp    (datum) (apply-k k (car datum))]
         [var-exp     (id)    (apply-env-cps env id (lambda () (eopl:error 'apply-env "variable ~s is not bound" id)) (var-k env k))]
         [app-exp     (stuff) (eval-exp-cps (car stuff) env (if (or (equal? (cadar stuff) 'or) (equal? (cadar stuff) 'and))
                                                                (app1-k (cdr stuff) env k)
                                                                (app2-k (cdr stuff) env k)))]
         [if1-exp     (condition arm0)      (eval-exp-cps condition env (if1-k arm0 env k))]
         [if2-exp     (condition arm0 arm1) (eval-exp-cps condition env (if2-k arm0 arm1 env k))]
         [letr-exp    (vars vbodies bodies) (eval-bodies-cps bodies (extend-env (map cadr vars) vbodies env) k)]
         [lambda-exp  (vars bodies) (apply-k k (closure vars bodies env))]
         [lambdai-exp (vars bodies) (apply-k k (closure vars bodies env))]
         [lambdal-exp (vars bodies) (apply-k k (closure vars bodies env))]
         [set!-exp    (target val)  (set-var-env env target (eval-exp-cps val env k))]
         [def-exp     (sym body)    (define-in-global sym (eval-exp body env))]
         [else        (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))

(define (eval-bodies-cps bodies env k)
  (if (null? (cdr bodies))
      (eval-exp-cps (car bodies) env k)
      (eval-exp-cps (car bodies) env (evbod-k (cdr bodies) env k))))

(define apply-proc-cps
  (lambda (proc-value args env k)
    (cases proc-val proc-value
           [prim-proc (op) (apply-prim-proc-cps op args env k)]
           [closure   (vars bodies env) (eval-bodies-cps bodies (extend-env vars args env) k)]
           [k-proc    (stored-k) (apply-k stored-k (car args))]
           [else      (eopl:error 'apply-proc
                                  "Attempt to apply bad procedure: ~s" 
                                  proc-value)])))

(define *prim-proc-names* '(+ - * / ++ add1 -- sub1 cons = < <= > >= not map or zero? list null? assq eq? equal? apply atom? length
                              list->vector list? pair? procedure? vector->list list-tail vector make-vector vector-ref vector? number?
                              eqv? symbol? set-car! set-cdr! vector-set! display append newline quotient car cdr caar cadr cdar cddr
                              caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                              cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))

(define init-env (extend-env *prim-proc-names* (map prim-proc *prim-proc-names*) (empty-env)))
(define global-env init-env)

;; Usually an interpreter must define each 
;; built-in procedure individually.  We are "cheating" a little bit.
(define (apply-prim-proc-cps prim-proc args env k)
  (apply-k k (case prim-proc
               [(+)      (apply + args)]
               [(-)      (apply - args)]
               [(*)      (apply * args)]
               [(/)      (apply / args)]
               [(=)      (apply = args)]
               [(<)      (apply < args)]
               [(>)      (apply > args)]
               [(<=)     (apply <= args)]
               [(>=)     (apply >= args)]
               [(++)     (+    (car args) 1)]
               [(--)     (-    (car args) 1)]
               [(or)     (ordered-ormap-cps args env k)]
               [(and)    (ordered-anmap-cps args env k)]
               [(eq?)    (eq?  (car args) (cadr args))]
               [(car)    (car  (car args))]
               [(cdr)    (cdr  (car args))]
               [(map)    (imap-cps (car args) (cdr args) env k)]
               [(not)    (not  (car args))]
               [(add1)   (+    (car args) 1)]
               [(sub1)   (-    (car args) 1)]
               [(cons)   (cons (car args) (cadr args))]
               [(eqv?)   (eqv? (car args) (cadr args))]
               [(list)   args]
               [(assq)   (assq (car args) (cadr args))]
               [(caar)   (caar (car args))]
               [(cadr)   (cadr (car args))]
               [(cdar)   (cdar (car args))]
               [(cddr)   (cddr (car args))]
               [(apply)  (apply-proc-cps (car args) (cadr args) env k)]
               [(atom?)  (and (not (pair (car args))) (not (null? (car args))))]
               [(null?)  (null? (car args))]
               [(list?)  (list? (car args))]
               [(pair?)  (pair? (car args))]
               [(caaar)  (caaar (car args))]
               [(caadr)  (caadr (car args))]
               [(cadar)  (cadar (car args))]
               [(caddr)  (caddr (car args))]
               [(cdaar)  (cdaar (car args))]
               [(cdadr)  (cdadr (car args))]
               [(cddar)  (cddar (car args))]
               [(cdddr)  (cdddr (car args))]
               [(zero?)  (zero? (car args))]
               [(append) (append (car args) (cadr args))]
               [(equal?) (equal? (car args) (cadr args))]
               [(length) (length (car args))]
               [(vector) (list->vector args)]
               [(caaaar) (caaaar (car args))]
               [(caaadr) (caaadr (car args))]
               [(caadar) (caadar (car args))]
               [(caaddr) (caaddr (car args))]
               [(cadaar) (cadaar (car args))]
               [(cadadr) (cadadr (car args))]
               [(caddar) (caddar (car args))]
               [(cadddr) (cadddr (car args))]
               [(cdaaar) (cdaaar (car args))]
               [(cdaadr) (cdaadr (car args))]
               [(cdadar) (cdadar (car args))]
               [(cdaddr) (cdaddr (car args))]
               [(cddaar) (cddaar (car args))]
               [(cddadr) (cddadr (car args))]
               [(cdddar) (cdddar (car args))]
               [(cddddr) (cddddr (car args))]
               [(vector?)      (vector? (car args))]
               [(number?)      (number? (car args))]
               [(symbol?)      (symbol? (car args))]
               [(display)      (display (car args))]
               [(newline)      (newline)]
               [(set-car!)     (set-car! (car args) (cadr args))]
               [(set-cdr!)     (set-cdr! (car args) (cadr args))]
               [(quotient)     (quotient (car args) (cadr args))]
               [(list-tail)    (list-tail (car args) (cadr args))]
               [(procedure?)   (and (list? (car args)) (or (equal? 'prim-proc (caar args)) (equal? 'closure (caar args))))]
               [(vector-ref)   (vector-ref (car args) (cadr args))]
               [(vector-set!)  (vector-set! (car args) (cadr args) (caddr args))]
               [(make-vector)  (make-vector (car args) (cadr args))]
               [(vector->list) (vector->list (car args))]
               [(list->vector) (list->vector (car args))]
               [else (error 'apply-prim-proc
                            "Bad primitive procedure name: ~s" 
                            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
