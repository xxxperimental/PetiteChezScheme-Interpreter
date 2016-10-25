;; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define (list-find-position sym los) (list-index (lambda (x) (eqv? sym x)) los))
(define (list-index pred ls)
  (cond [(null? ls) #f]
        [(pred (car ls)) 0]
        [else (let ([list-index-r (list-index pred (cdr ls))])
                (if (number? list-index-r)
                    (+ 1 list-index-r)
                    #f))]))

(define (empty-env) '())
(define (extend-env syms vals env) (v syms (list->vector vals) env))
(define (apply-env env sym fail)
  (let helper ([env env] [sym sym] [fail fail] [global #f])
    (if (null? env)
        (if global (fail) (helper global-env sym fail #t))
        (let ([pos (list-find-position sym (v0 env))])
          (if (number? pos)
              (vr (v1 env) pos)
              (helper (v2 env) sym fail global))))))

