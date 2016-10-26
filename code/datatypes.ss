;; Parsed expression datatypes
(define-datatype expression expression?
  [var-exp (id symbol?)]  ; variable references
  [lit-exp (datum         ; "Normal" data.  Did I leave out any types?
            (lambda (x) (ormap
                         (lambda (pred) (pred x))
                         (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp (stuff (list-of expression?))]  ; applications
  [litq-exp (lit list?)]
  [lambda-exp (vars list?) (body (list-of expression?))]
  [lambdai-exp (vars pair?) (body (list-of expression?))]
  [lambdal-exp (vars list?) (body (list-of expression?))]
  [begin-exp (exp list?)]
  [if1-exp  (condition expression?) (arm0 expression?)]
  [if2-exp  (condition expression?) (arm0 expression?) (arm1 expression?)]
  [cond-exp (exp list?)]
  [let-exp  (varnames list?) (varexps list?) (body list?)]
  [letn-exp (id symbol?) (varnames list?) (varexps list?) (body list?)]
  [let*-exp (varnames list?) (varexps list?) (body list?)]
  [letr-exp (varnames list?) (varexps list?) (body list?)]
  [set!-exp (target symbol?) (val expression?)]
  ;;******************************\/\/\/\/******************************;;
  [case-lambda-exp (lambda-exps (list-of expression?))]
  ;;******************************/\/\/\/\******************************;;
  )

;; datatype for procedures.  At first there is only one
;; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc (name symbol?)]
  [closure (vars (list-of symbol?))
           (bodies list?)
           (type symbol?) ; Should be 'n' for normal, 'i' for improper, and 'l' for list
           (env (lambda (x) (ormap
                             (lambda (pred) (pred x))
                             (list vector? null?))))]
  ;;******************************\/\/\/\/******************************;;
  [case-closure (closures (list-of proc-val?))]
  ;;******************************/\/\/\/\******************************;;
  )

;; environment type definitions
(define (scheme-value? x) #t)

