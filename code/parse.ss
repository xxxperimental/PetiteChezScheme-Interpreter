;; Updated Assignment 11b for interpreter
;; Wesley Van Pelt & Maxim Zaman

(define (get-let-vars code vars) (map (lambda (x) (parse-exp (car x) vars #f)) code))
(define (get-let-varexps code vars) (map (lambda (x) (parse-exp (cadr x) vars)) code))
(define (unparse-let-vars varnames varexps)
  (map (lambda (x y) (list x y))
       (map (lambda (x) (unparse-exp x)) varnames)
       (map (lambda (x) (unparse-exp x)) varexps)))

(define (get-address sym vars)
  (let h1 ([vars vars] [depth 0])
    (if (null? vars)
        (eopl:error 'get-address "Shits on fire yo")
        (let ([ret (let h2 ([current-vars (car vars)] [pos 0])
                     (cond [(null? current-vars) #f]
                           [(equal? (car current-vars) sym) (cons depth pos)]
                           [else (h2 (cdr vars) (++ pos))]))])
          (if ret ret (h1 (cdr vars) (++ depth)))))))

(define parse-exp
  (lambda (datum vars . b)
    (cond [(symbol? datum)  (if (and (not (null? b)) (not (car b)))
                                (var-exp datum)
                                (let ([address (get-address datum vars)])
                                  (varl-exp (car address) (cdr address))))]
          [(boolean? datum) (lit-exp datum)]
          [(number? datum)  (lit-exp datum)]
          [(string? datum)  (lit-exp datum)]
          [(vector? datum)  (lit-exp datum)]
          [(pair? datum)
           (cond [(eqv? (car datum) 'lambda)
                  (if (>= (length datum) 3)
                      (cond [(list? (cadr datum))
                             (if (andmap symbol? (cadr datum))
                                 (lambda-exp (cadr datum) (map parse-exp (cddr datum) (cons (list (cadr datum)) vars)))
                                 (eopl:error 'parse-exp "args must be symbols in lambda"))]
                            [(and (pair? (cadr datum)) (not (list? (cadr datum))))
                             (if (andmap symbol? (to-proper (cadr datum)))
                                 (lambdai-exp (to-proper (cadr datum)) (map parse-exp (cddr datum) (cons (cadr datum) vars)))
                                 (eopl:error 'parse-exp "args must be symbols in lambda"))]
                            [else
                             (if (symbol? (cadr datum))
                                 (lambdal-exp (list (cadr datum)) (map parse-exp (cddr datum) (cons (cadr datum) vars)))
                                 (eopl:error 'parse-exp "arg must be a symbol in lambda"))])
                      (eopl:error 'parse-exp "incorrect number of parameters for the lambda"))]
                 [(eqv? (car datum) 'if)
                  (if (and (<= (length datum) 4) (>= (length datum) 3))
                      (if (null? (cdddr datum))
                          (if1-exp (parse-exp (cadr datum) vars) (parse-exp (caddr datum) vars))
                          (if2-exp (parse-exp (cadr datum) vars) (parse-exp (caddr datum) vars) (parse-exp (cadddr datum) vars)))
                      (eopl:error 'parse-exp "incorrect if length"))]
                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 ;; TODO: add error checking to all of this new stuff? ;;
                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 [(eqv? (car datum) 'cond)   (cond-exp  (map parse-exp (cdr datum) vars))]
                 [(eqv? (car datum) 'begin)  (begin-exp (map parse-exp (cdr datum) (cons '() vars)))]
                 [(eqv? (car datum) 'while)  (while-exp (parse-exp (cadr datum) vars) (map parse-exp (cddr datum) (cons '() vars)))]
                 [(eqv? (car datum) 'define) (def-exp   (cadr datum) (parse-exp (caddr datum) vars))]
                 [(eqv? (car datum) 'case)   (case-exp  (parse-exp (cadr datum) vars)
                                                        (map (lambda (x) (parse-exp (car  x) vars)) (cddr datum))
                                                        (map (lambda (x) (parse-exp (cadr x) vars)) (cddr datum)))]
                 ;;;;;;;;;;;;;;;;;;;
                 ;; End new stuff ;;
                 ;;;;;;;;;;;;;;;;;;;
                 [(eqv? (car datum) 'let)
                  (if (>= (length datum) 3)
                      (if (symbol? (cadr datum))
                          (if (list? (caddr datum))
                              (if (and (andmap (lambda (x) (and (list? x) (equal? (length x) 2))) (caddr datum))
                                       (andmap symbol? (map car (caddr datum))))
                                  (letn-exp (cadr datum) (get-let-vars (caddr datum) vars) (get-let-varexps (caddr datum) vars) (parse-exp (cdddr datum) (cons (get-let-vars (caddr datum) vars) vars)))
                                  (eopl:error 'parse-exp "all list variables must be symbols"))
                              (eopl:error 'parse-exp "arguments list must be a proper list"))
                          (if (list? (cadr datum))
                              (if (and (andmap (lambda (x) (and (list? x) (equal? (length x) 2))) (cadr datum))
                                       (andmap symbol? (map car (cadr datum))))
                                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                  ;; FIXME?: Fix the rest of the let parsing (maybe?) ;;
                                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                  (let-exp (get-let-vars (cadr datum) vars) (get-let-varexps (cadr datum) vars) (map (lambda (x) (parse-exp x (cons (get-let-vars (cadr datum) vars) vars))) (cddr datum)))
                                  (eopl:error 'parse-exp "all list variables must be symbols"))
                              (eopl:error 'parse-exp "arguments list must be a proper list")))
                      (eopl:error 'parse-exp "incorrect number of parameters for the let"))]
                 [(eqv? (car datum) 'let*)
                  (if (>= (length datum) 3)
                      (if (list? (cadr datum))
                          (if (and (andmap (lambda (x) (and (list? x) (equal? (length x) 2))) (cadr datum))
                                   (andmap symbol? (map car (cadr datum))))
                              (let*-exp (get-let-vars (cadr datum) vars) (get-let-varexps (cadr datum) vars) (map (lambda (x) (parse-exp x (cons (get-let-vars (cadr datum) vars) vars))) (cddr datum)))
                              (eopl:error 'parse-exp "all list variables must be symbols"))
                          (eopl:error 'parse-exp "all list variables must be symbols"))
                      (eopl:error 'parse-exp "incorrect number of parameters for the let*"))]
                 [(eqv? (car datum) 'letrec)
                  (if (>= (length datum) 3)
                      (if (list? (cadr datum))
                          (if (and (andmap (lambda (x) (and (list? x) (equal? (length x) 2))) (cadr datum))
                                   (andmap symbol? (map car (cadr datum))))
                              (letr-exp (get-let-vars (cadr datum) vars) (get-let-varexps (cadr datum) vars) (map (lambda (x) (parse-exp x (cons (get-let-vars (cadr datum) vars) vars))) (cddr datum)))
                              (eopl:error 'parse-exp "all list variables must be symbols"))
                          (eopl:error 'parse-exp "all list variables must be symbols"))
                      (eopl:error 'parse-exp "incorrect number of parameters for the letrec"))]
                 [(eqv? (car datum) 'set!)
                  (if (= (length datum) 3)
                      (set!-exp (cadr datum) (parse-exp (caddr datum) vars))
                      (eopl:error 'parse-exp "incorrect number of parameters for the set!"))]
                 [(eqv? (car datum) 'quote)
                  (litq-exp (list (cadr datum)))]
                 [else
                  (if (list? datum)
                      (let ([px (parse-exp (car datum) vars)])
                        (cond [(equal? 'lambdal-exp (car px))
                               (begin (dpp "ASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSA")
                                      (app-exp (list px (app-exp (cons (var-exp 'list) (map (lambda (x) (parse-exp x vars)) (cdr datum)))))))]
                              [(equal? 'lambdai-exp (car px))
                               (begin (dpp "LKJHGFDSASDFGHJKLLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKLKJHGFDSASDFGHJKL")
                                      (app-exp (append (append (list px)
                                                        (let helper ([args (cdr datum)] [count (-- (length (cadr px)))])
                                                          (if (= 0 count) '() (cons (parse-exp (car args) vars) (helper (cdr args) (-- count))))))
                                                (list (app-exp (cons (var-exp 'list)
                                                                     (map (lambda (x) (parse-exp x vars))
                                                                          (list-tail (cdr datum) (-- (length (cadr px)))))))))))]
                              [else (app-exp (map (lambda (x) (parse-exp x vars)) datum))]))
                      (eopl:error 'parse-exp "expression must be a proper list"))])]
          [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

;; This may be broken
(define unparse-exp
  (lambda (ex)
    (cases expression ex
           [var-exp (id) id]
           [lit-exp (lit) lit]
           [litq-exp (lit) `(quote,(car lit))]
           [app-exp (stuffs)
                    (map (lambda (x) (unparse-exp x)) stuffs)]
           [lambda-exp (vars body)
                       (append (list 'lambda vars) (unparse-exp body))]
           [lambdai-exp (vars body)
                        (append (list 'lambda vars) (unparse-exp body))]
           [lambdal-exp (var body)
                        (append (list 'lambda var) (unparse-exp body))]
           [if1-exp (condition arm0)
                    (list 'if (unparse-exp condition) (unparse-exp arm0))]
           [if2-exp (condition arm0 arm1)
                    (list 'if (unparse-exp condition) (unparse-exp arm0) (unparse-exp arm1))]
           [let-exp (varnames varexps body)
                    (append (list 'let (unparse-let-vars varnames varexps)) (unparse-exp body))]
           [letn-exp (id varnames varexps body)
                     (append (list 'let id (unparse-let-vars varnames varexps)) (unparse-exp body))]
           [let*-exp (varnames varexps body)
                     (append (list 'let* (unparse-let-vars varnames varexps)) (unparse-exp body))]
           [letr-exp (varnames varexps body)
                     (append (list 'letrec (unparse-let-vars varnames varexps)) (unparse-exp body))]
           [set!-exp (target val)
                     (list 'set! target (unparse-exp val))])))
