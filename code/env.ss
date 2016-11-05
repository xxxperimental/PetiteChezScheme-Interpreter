;; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define (list-find-position sym los) (list-index (lambda (x) (eqv? sym x)) los))
(define (list-index pred ls)
  (cond [(null? ls) #f]
        [(pred (car ls)) 0]
        [else (let ([list-index-r (list-index pred (cdr ls))])
                (if (number? list-index-r)
                    (+ 1 list-index-r)
                    #f))]))
(define (reset-global-env) (set! global-env init-env))

(define (empty-env) (v))
(define (extend-env syms vals env) (v syms (list->vector vals) env))
(define (apply-env-cps env sym fail k)
  (let helper ([env env] [global #f] [k k])
    (if (= (vl env) 0)
        (if global (fail) (helper global-env #t k))
        (let ([pos (list-find-position sym (v0 env))])
          (if (number? pos)
              (vr (v1 env) pos)
              (helper (v2 env) global k))))))
(define (set-var-env env sym val)
  (let helper ([env env] [global #f])
    (if (= (vl env) 0)
        (helper global-env #t)
        (let ([pos (list-find-position sym (v0 env))])
          (cond [(number? pos)    (vs (v1 env) pos val)]
                [global           (begin (append! (v0 global-env) (list sym))
                                         (vs global-env 1 (va (v1 global-env) val)))]
                [(null? (v2 env)) (helper global-env #t)]
                [else             (helper (v2 env) #f)])))))
(define (define-in-global sym val)
  (let ([pos (list-find-position sym (v0 global-env))])
    (if (number? pos)
        (vs (v1 global-env) pos val)
        (begin (append! (v0 global-env) (list sym))
               (vs global-env 1 (va (v1 global-env) val))))))
