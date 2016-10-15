;; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    ;; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

(define (eval-exp-helper ex env)
  (if (and (list? ex) (not (null? ex)) (or (equal? (car ex) 'lit-exp) (equal? (car ex) 'var-exp) (equal? (car ex) 'app-exp)
                                           (equal? (car ex) 'if2-exp) (equal? (car ex) 'let-exp) (equal? (car ex) 'lambda-exp)))
      (eval-exp ex env)
      ex))

;; eval-exp is the main component of the interpreter
(define eval-exp
  (lambda (exp env)
    ;(dpp exp)
    (cases expression exp
           [lit-exp    (datum) datum]
           [litq-exp   (datum) (car datum)]
           [var-exp    (id)    (eval-exp-helper (apply-env env id (lambda () (eopl:error 'apply-env "variable ~s is not bound" id))) env)]
           [app-exp    (stuff) (apply-proc (eval-exp (car stuff) env)
                                         (map (lambda (x) (eval-exp x env)) (cdr stuff))
                                         env)]
           [if2-exp    (condition arm0 arm1) (if (eval-exp condition env)
                                                 (eval-exp arm0 env)
                                                 (eval-exp arm1 env))]
           [let-exp    (varnames varexps body) (eval-bodies
                                                body
                                                (extend-env (map cadr varnames) (map (lambda (x) (eval-exp-helper x env)) varexps) env))]
           [lambda-exp (vars bodies) (closure vars bodies env)]
           [else       (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))


(define (eval-bodies bodies env)
  (if (null? (cdr bodies))
      (eval-exp (car bodies) env)
      (begin (eval-exp (car bodies) env)
             (eval-bodies (cdr bodies) env))))

(define apply-proc
  (lambda (proc-value args env)
    (cases proc-val proc-value
           [prim-proc (op) (apply-prim-proc op args env)]
           [closure (vars bodies env) (eval-bodies bodies (extend-env vars args env))]
           [else (eopl:error 'apply-proc
                             "Attempt to apply bad procedure: ~s" 
                             proc-value)])))

(define *prim-proc-names* '(+ - * / ++ add1 -- sub1 cons = < <= > >= not
                              zero? list null? assq eq? equal? atom? length
                              list->vector list? pair? procedure? vector->list
                              vector make-vector vector-ref vector? number?
                              symbol? set-car! set-cdr! vector-set! display
                              newline car cdr caar cadr cdar cddr caaar
                              caadr cadar caddr cdaar cdadr cddar cdddr
                              caaaar caaadr caadar caaddr cadaar cadadr
                              caddar cadddr cdaaar cdaadr cdadar cdaddr
                              cddaar cddadr cdddar cddddr))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*     ; a value (not an expression) with an identifier.
   (map prim-proc *prim-proc-names*)
   (empty-env)))
(define global-env init-env)


;; Usually an interpreter must define each 
;; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args env)
    (case prim-proc
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
      [(eq?)    (eq?  (car args) (cadr args))]
      [(car)    (car  (car args))]
      [(cdr)    (cdr  (car args))]
      [(not)    (not  (car args))]
      [(add1)   (+    (car args) 1)]
      [(sub1)   (-    (car args) 1)]
      [(cons)   (cons (car args) (cadr args))]
      [(list)   args]
      [(assq)   (assq (car args) (cadr args))]
      [(caar)   (caar (car args))]
      [(cadr)   (cadr (car args))]
      [(cdar)   (cdar (car args))]
      [(cddr)   (cddr (car args))]
      [(null?)  (null? (car args))]
      [(atom?)  (and (not (pair (car args))) (not (null? (car args))))]
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
      [(display)      (display (car args) (cadr args))]
      [(newline)      (newline)]
      [(set-car!)     (set-car! (car args) (cadr args))]
      [(set-cdr!)     (set-cdr! (car args) (cadr args))]
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
  (lambda (x) (top-level-eval (parse-exp x))))
