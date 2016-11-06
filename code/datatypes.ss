;; Parsed expression datatypes
(define-datatype expression expression?
  [var-exp     (id symbol?)]
  [lit-exp     (datum (lambda (x) (ormap (lambda (pred) (pred x))
                                         (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp     (stuff (list-of expression?))]  ; applications
  [litq-exp    (lit list?)]
  [lambda-exp  (vars list?) (body (list-of expression?))]
  [lambdai-exp (vars pair?) (body (list-of expression?))]
  [lambdal-exp (vars list?) (body (list-of expression?))]
  [begin-exp   (exp list?)]
  [if1-exp     (condition expression?) (arm0 expression?)]
  [if2-exp     (condition expression?) (arm0 expression?) (arm1 expression?)]
  [while-exp   (condition expression?) (bodies list?)]
  [case-exp    (val expression?) (conds list?) (bodies list?)]
  [cond-exp    (exp list?)]
  [let-exp     (varnames list?) (varexps list?) (body list?)]
  [letn-exp    (id symbol?) (varnames list?) (varexps list?) (body list?)]
  [let*-exp    (varnames list?) (varexps list?) (body list?)]
  [letr-exp    (varnames list?) (varexps list?) (body list?)]
  [set!-exp    (target symbol?) (val expression?)]
  [def-exp     (id symbol?) (body expression?)])

(define-datatype proc-val proc-val?
  [prim-proc (name symbol?)]
  [k-proc    (stored continuation?)]
  [closure   (vars (list-of symbol?))
             (bodies list?)
             (env (lambda (x) (ormap
                               (lambda (pred) (pred x))
                               (list vector? null?))))])

(define-datatype cont continuation?
  [id-k    (x (lambda (t) #t))]
  [var-k   (env vector?) (k continuation?)]
  [app1-k  (args list?) (env vector?) (k continuation?)]
  [app2-k  (args list?) (env vector?) (k continuation?)]           
  [app2-kk (vv (lambda (t) #t)) (env vector?) (k continuation?)]
  [rator-k (rands (list-of expression?)) (env vector?) (k continuation?)]
  [if1-k   (then-exp expression?) (env vector?) (k continuation?)]
  [if2-k   (then-exp expression?) (else-exp expression?) (env vector?) (k continuation?)]
  [rands-k (rator-val proc-val?) (k continuation?)]
  [let-k   (vars list?) (bodies list?) (env vector?) (k continuation?)]
  [ormap-k (lst list?) (env vector?) (k continuation?)]
  [anmap-k (lst list?) (env vector?) (k continuation?)]
  [procm-k (maptype (lambda (t) #t)) (k continuation?)]
  [evbod-k (bodies list?) (env vector?) (k continuation?)])

 
