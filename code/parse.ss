;; Updated Assignment 11b for interpreter
;; Wesley Van Pelt & Maxim Zaman

(define (get-let-vars code) (map (lambda (x) (parse-exp (car x))) code))
(define (get-let-varexps code) (map (lambda (x) (parse-exp (cadr x))) code))
(define (unparse-let-vars varnames varexps)
  (map (lambda (x y) (list x y))
       (map (lambda (x) (unparse-exp x)) varnames)
       (map (lambda (x) (unparse-exp x)) varexps)))

(define parse-exp
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
          [(boolean? datum) (lit-exp datum)]
          [(number? datum) (lit-exp datum)]
          [(string? datum) (lit-exp datum)]
          [(vector? datum) (lit-exp datum)]
          [(pair? datum)
           (cond [(eqv? (car datum) 'lambda)
                  (if (>= (length datum) 3)
                      (cond [(list? (cadr datum))
                             (if (andmap symbol? (cadr datum))
                                 (lambda-exp (cadr datum) (map parse-exp (cddr datum)))
                                 (eopl:error 'parse-exp "args must be symbols in lambda"))]
                            [(and (pair? (cadr datum)) (not (list? (cadr datum))))
                             (if (andmap symbol? (to-proper (cadr datum)))
                                 (lambdai-exp (to-proper (cadr datum)) (map parse-exp (cddr datum)))
                                 (eopl:error 'parse-exp "args must be symbols in lambda"))]
                            [else
                             (if (symbol? (cadr datum))
                                 (lambdal-exp (list (cadr datum)) (map parse-exp (cddr datum)))
                                 (eopl:error 'parse-exp "arg must be a symbol in lambda"))])
                      (eopl:error 'parse-exp "incorrect number of parameters for the lambda"))]
                 [(eqv? (car datum) 'if)
                  (if (and (<= (length datum) 4) (>= (length datum) 3))
                      (if (null? (cdddr datum))
                          (if1-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)))
                          (if2-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum))))
                      (eopl:error 'parse-exp "incorrect if length"))]
                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 ;; FIXME: add error checking to all of this new stuff ;;
                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 [(eqv? (car datum) 'cond)
                  (cond-exp (map parse-exp (cdr datum)))]
                 [(eqv? (car datum) 'begin)
                  (begin-exp (map parse-exp (cdr datum)))]
                 [(eqv? (car datum) 'let)
                  (if (>= (length datum) 3)
                      (if (symbol? (cadr datum))
                          (if (list? (caddr datum))
                              (if (and (andmap (lambda (x) (and (list? x) (equal? (length x) 2))) (caddr datum))
                                       (andmap symbol? (map car (caddr datum))))
                                  (letn-exp (cadr datum) (get-let-vars (caddr datum)) (get-let-varexps (caddr datum)) (parse-exp (cdddr datum)))
                                  (eopl:error 'parse-exp "all list variables must be symbols"))
                              (eopl:error 'parse-exp "arguments list must be a proper list"))
                          (if (list? (cadr datum))
                              (if (and (andmap (lambda (x) (and (list? x) (equal? (length x) 2))) (cadr datum))
                                       (andmap symbol? (map car (cadr datum))))
                                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                  ;; FIXME: Fix the rest of the let parsing ;;
                                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                  (let-exp (get-let-vars (cadr datum)) (get-let-varexps (cadr datum)) (map parse-exp (cddr datum)))
                                  (eopl:error 'parse-exp "all list variables must be symbols"))
                              (eopl:error 'parse-exp "arguments list must be a proper list")))
                      (eopl:error 'parse-exp "incorrect number of parameters for the let"))]
                 [(eqv? (car datum) 'let*)
                  (if (>= (length datum) 3)
                      (if (list? (cadr datum))
                          (if (and (andmap (lambda (x) (and (list? x) (equal? (length x) 2))) (cadr datum))
                                   (andmap symbol? (map car (cadr datum))))
                              (let*-exp (get-let-vars (cadr datum)) (get-let-varexps (cadr datum)) (parse-exp (cddr datum)))
                              (eopl:error 'parse-exp "all list variables must be symbols"))
                          (eopl:error 'parse-exp "all list variables must be symbols"))
                      (eopl:error 'parse-exp "incorrect number of parameters for the let*"))]
                 [(eqv? (car datum) 'letrec)
                  (if (>= (length datum) 3)
                      (if (list? (cadr datum))
                          (if (and (andmap (lambda (x) (and (list? x) (equal? (length x) 2))) (cadr datum))
                                   (andmap symbol? (map car (cadr datum))))
                              (letr-exp (get-let-vars (cadr datum)) (get-let-varexps (cadr datum)) (parse-exp (cddr datum)))
                              (eopl:error 'parse-exp "all list variables must be symbols"))
                          (eopl:error 'parse-exp "all list variables must be symbols"))
                      (eopl:error 'parse-exp "incorrect number of parameters for the letrec"))]
                 [(eqv? (car datum) 'set!)
                  (if (= (length datum) 3)
                      (set!-exp (cadr datum) (parse-exp (caddr datum)))
                      (eopl:error 'parse-exp "incorrect number of parameters for the set!"))]
                 [(eqv? (car datum) 'quote)
                  (litq-exp (list (cadr datum)))]
                 [else
                  (if (list? datum)
                      (let ([px (parse-exp (car datum))])
                        (cond [(equal? 'lambdal-exp (car px))
                               (app-exp (list px (app-exp (cons (var-exp 'list) (map (lambda (x) (parse-exp x)) (cdr datum))))))]
                              [(equal? 'lambdai-exp (car px))
                               (app-exp (append (append (list px)
                                                        (let helper ([args (cdr datum)] [count (-- (length (cadr px)))])
                                                          (if (= 0 count) '() (cons (parse-exp (car args)) (helper (cdr args) (-- count))))))
                                                (list (app-exp (cons (var-exp 'list)
                                                                     (map (lambda (x) (parse-exp x))
                                                                          (list-tail (cdr datum) (-- (length (cadr px))))))))))]
                              [else (app-exp (map (lambda (x) (parse-exp x)) datum))]))
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
