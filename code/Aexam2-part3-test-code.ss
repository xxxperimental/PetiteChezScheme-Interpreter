;; Test code for CSSE 304 Exam 2 part 3 201710

(define (test-C3)
    (let ([correct '(
		     ((1 1 2 6 24 120)(1 1 2 6 24 120))
		     ((k e b a d c) (k e b a d c))
		     ((#t #f #f #t) (#t #f #f #t))
		     )]
          [answers 
            (list 
		(list (eval-one-exp ' 
		       (letrec ([fact (lambda (x) 
					(if (zero? x) 
					    1
					    (* x (fact (- x 1)))))]) 
			 (map fact '(0 1 2 3 4 5)))) 
		      (eval-one-exp ' 
		       (letrek ([fact (lambda (x) 
					(if (zero? x) 
					    1 
					    (* x (fact (- x 1)))))]) 
			       (map fact '(0 1 2 3 4 5)))))
		(list (eval-one-exp ' 
		       (letrec ([union (lambda (s1 s2) 
					 (cond [(null? s1) s2] 
					       [(member? (car s1) s2) 
						(union (cdr s1) s2)] 
					       [else (cons (car s1) (union (cdr s1) s2))]))] 
				[member? (lambda (sym ls) 
					   (cond [(null? ls) #f] 
						 [(eqv? (car ls) sym) #t] 
						 [else (member? sym (cdr ls))]))]) 
			 (union '(a c e d k) '(e b a d c))))
		      (eval-one-exp ' 
		       (letrek ([union (lambda (s1 s2) 
					 (cond [(null? s1) s2] 
					       [(member? (car s1) s2) 
						(union (cdr s1) s2)] 
					       [else (cons (car s1) (union (cdr s1) s2))]))] 
				[member? (lambda (sym ls) 
					   (cond [(null? ls) #f] 
						 [(eqv? (car ls) sym) #t] 
						 [else (member? sym (cdr ls))]))]) 
			       (union '(a c e d k) '(e b a d c)))))
		(list (eval-one-exp ' 
		       (letrec ([even? (lambda (n) 
					 (if (zero? n) #t (odd? (- n 1))))] 
				[odd? (lambda (m) (if (zero? m) #f (even? (- m 1))))]) 
			 (list (odd? 3) (even? 3) (odd? 4) (even? 4)))) 
		      (eval-one-exp ' 
		       (letrek ([even? (lambda (n) (if (zero? n) #t (odd? (- n 1))))] 
				[odd? (lambda (m) (if (zero? m) #f (even? (- m 1))))]) 
			       (list (odd? 3) (even? 3) (odd? 4) (even? 4)))))
	     )])
      (display-results correct answers equal?)))

(define (test-C4)
    (let ([correct '(
		     7
		     23
		     23
		     23
		     ((()) 6 21 25)
		     19
		     )]
          [answers 
            (list 
	     (eval-one-exp '((case-lambda [(n) (+ n 2)] [(m n) (+ m n 10)]) 5))
	     (eval-one-exp '((case-lambda [(n) (+ n 2)] [(m n) (+ m n 10)]) 5 8))
	     (eval-one-exp '((case-lambda [(n) (+ n 2)] [(m . n) (+ m (car n) 10)]) 5 8))
	     (eval-one-exp '((case-lambda [(m . n) (+ m (car n) 10)] [(n) (+ n 2)]) 5 8))
	     (eval-one-exp '(let ([f 
				   (case-lambda [(n) (+ n 2)] [(m . n) (+ m (car n) 10)] 
						[L (list L)])]) (list (f) (f 4) (f 5 6) (f 7 8 9) )))
	     (eval-one-exp '(let ([f (case-lambda 
				      [(n) (+ n 2)] 
				      [(m . n) (+ m (car n) 10)] 
				      [L (list L)])]) (f (f 3 4 5))))

	     )])
      (display-results correct answers equal?)))





;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))


(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
	  (cadar graph)
	  (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
	#t
	(if (member (car list) (cdr list))
	    #f
	    (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
	 (let ([syms (map car obj)])
	   (and (set? syms)
		(andmap symbol? syms)
		(andmap (lambda (x)
			  (andmap (lambda (y) (member y (remove (car x) syms)))
				  (cadr x)))
			obj))))))
    
(define graph-equal?
  (lambda (a b)
    (and
     (graph? a) 
     (graph? b)
     (let ([a-nodes (map car a)]
	   [b-nodes (map car b)])
       (and 
	(set-equals? a-nodes b-nodes)
	    ; Now  See if the edges from each node are equivalent in the two graphs.
	(let loop ([a-nodes a-nodes])
	  (if (null? a-nodes)
	      #t
	      (let ([a-edges (find-edges a (car a-nodes))]
		    [b-edges (find-edges b (car a-nodes))])
		(and (set-equals? a-edges b-edges)
		     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
		 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)
	   
	  
     



;You can run the tests individually, or run them all
;#by loading this file (and your solution) and typing (r)

(define (run-all)
  ;(display 'C3) 
  ;(test-C3 )
  (display 'C4) 
  (test-C4)

)

(define r run-all)

