;; Parsed expression datatypes
(define-datatype expression expression?
  [var-exp (id symbol?)]
  [lit-exp (datum (lambda (x) (ormap
                               (lambda (pred) (pred x))
                               (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp     (stuff (list-of expression?))]  ; applications
  [litq-exp    (lit list?)]
  [lambda-exp  (vars list?) (body (list-of expression?))]
  [lambdai-exp (vars pair?) (body (list-of expression?))]
  [lambdal-exp (vars list?) (body (list-of expression?))]
  [begin-exp (exp list?)]
  [if1-exp   (condition expression?) (arm0 expression?)]
  [if2-exp   (condition expression?) (arm0 expression?) (arm1 expression?)]
  [while-exp (condition expression?) (bodies list?)]
  [case-exp  (val expression?) (conds list?) (bodies list?)]
  [cond-exp  (exp list?)]
  [let-exp   (varnames list?) (varexps list?) (body list?)]
  [letn-exp  (id symbol?) (varnames list?) (varexps list?) (body list?)]
  [let*-exp  (varnames list?) (varexps list?) (body list?)]
  [letr-exp  (varnames list?) (varexps list?) (body list?)]
  [set!-exp  (target symbol?) (val expression?)])

;; datatype for procedures.  At first there is only one
;; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc (name symbol?)]
  [closure (vars (list-of symbol?))
           (bodies list?)
           (env (lambda (x) (ormap
                             (lambda (pred) (pred x))
                             (list vector? null?))))])

;; environment type definitions
(define (scheme-value? x) #t)

