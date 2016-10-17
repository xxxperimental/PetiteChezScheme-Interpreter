;; General use fucntions
(define vr vector-ref)
(define (++ x) (+ x 1))
(define (-- x) (- x 1))
(define (dp x) (display x))
(define (dpp x) (display x)(newline))
(define br newline)

(define (imap proc lists env)
  (letrec ([helper (lambda (lists)
                     (if (or (null? lists) (null? (car lists))) '()
                         (cons (apply-proc proc (map car lists) env)
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
