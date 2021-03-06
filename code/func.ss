;; General use fucntions
(define v vector)
(define vr vector-ref)
(define vs vector-set!)
(define vl vector-length)
(define (v0 v) (vr v 0))
(define (v1 v) (vr v 1))
(define (v2 v) (vr v 2))
(define (v3 v) (vr v 3))
(define (++ x) (+ x 1))
(define (-- x) (- x 1))
(define (pa x) (parse-exp x))
(define (pe x) (syntax-expand (parse-exp x)))
(define (dp x) (display x))
(define (dpp x) (display x)(newline))
(define br newline)

(define map-cps ; Pass in a proc-cps
  (lambda (proc ls k)
    (if (null? ls)
        (apply-k k '())
        (map-cps proc (cdr ls)
                 (id-k (lambda (map-cps-cdr)
                         (proc (car ls)
                               (id-k (lambda (map-cps-car)
                                       (apply-k k (cons map-cps-car map-cps-cdr)))))))))))

(define (vector-append-val vec val)
  (let ([nvec (make-vector (++ (vl vec)))])
    (do ([i 0 (++ i)]) ([= i (vl vec)] nvec) (vs nvec i (vr vec i)))
    (vs nvec (vl vec) val)
    nvec))
(define va vector-append-val)

(define (imap-cps proc lists env k)
  (letrec ([helper (lambda (lists)
                     (if (or (null? lists) (null? (car lists)))
                         (apply-k k '())
                         (cons (apply-proc-cps proc (map car lists) env k)
                               (helper (map cdr lists)))))])
    (if (and (list? lists)
             (list? (car lists))
             (andmap (lambda (x) (equal? (length x) (length (car lists)))) lists))
        (helper lists)
        (eopl:error 'map "Invalid arguments to map"))))

(define (to-proper ls)
  (if (list? ls) ls
      (if (pair? ls)
          (cons (car ls) (to-proper (cdr ls)))
          (cons ls '()))))

(define syntax-expand
  (lambda (exp)
    (cases expression exp
           [let-exp   (varnames varexps body)
                      (app-exp (append (list (list 'lambda-exp (map cadr varnames) (map syntax-expand body)))
                                       (map syntax-expand varexps)))]
           [let*-exp  (varnames varexps body)                      
                      (if (equal? (length varnames) 1)
                          (syntax-expand (let-exp (list (car varnames)) (list (car varexps)) body))
                          (syntax-expand (let-exp (list (car varnames)) (list (car varexps)) (list (let*-exp (cdr varnames) (cdr varexps) body)))))]
           [letn-exp  (name vars varexps bodies)
                      (letr-exp (list (list 'var-exp name))
                                (list (list 'lambda-exp (map cadr vars) (map syntax-expand (cadr bodies))))
                                (list (list 'app-exp (cons (list 'var-exp name) (map syntax-expand varexps)))))]
           [cond-exp  (exps)
                      (syntax-expand (let helper ([es exps])
                                       (cond [(equal? (cadar (cadar es)) 'else) (cadr (cadar es))]
                                             [(null? (cdr es)) (if1-exp (car (cadar es)) (cadr (cadar es)))]
                                             [else (if2-exp (car (cadar es)) (cadr (cadar es)) (helper (cdr es)))])))]
           [begin-exp (exps)
                      (syntax-expand (app-exp (list (lambda-exp '() exps))))]
           [while-exp (cnd bodies)
                      (letr-exp (list (var-exp 'looper) (var-exp 'bodies))
                                (list (lambda-exp '() (list (if1-exp (syntax-expand cnd)
                                                                     (app-exp (list (lambda-exp '() (append bodies (list (app-exp (list (var-exp 'looper))))))))))))
                                (list (app-exp (list (var-exp 'looper)))))]
;           [case-exp  (val conds condexps)
;                      (syntax-expand (cond-exp (map (lambda (x) (app-exp (list (cadr datum) (
           ;; Just recurse down these
           [app-exp     (stuff) (app-exp (map syntax-expand stuff))]
           [if1-exp     (condition arm0) (if1-exp (syntax-expand condition) (syntax-expand arm0))]
           [if2-exp     (condition arm0 arm1) (if2-exp (syntax-expand condition) (syntax-expand arm0) (syntax-expand arm1))]
           [letr-exp    (vars vbodies bodies) (letr-exp vars (map syntax-expand vbodies) (map syntax-expand bodies))]
           [lambda-exp  (vars bodies) (lambda-exp vars (map syntax-expand bodies))]
           [lambdai-exp (vars bodies) (lambda-exp vars (map syntax-expand bodies))]
           [lambdal-exp (vars bodies) (lambda-exp vars (map syntax-expand bodies))]
           [set!-exp    (target val)  (set!-exp target (syntax-expand val))]
           [def-exp     (sym body)    (def-exp sym (syntax-expand body))]
           [else exp])))

