#lang racket

(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x) ) ) ) )

(define a-pair?
  (λ (x)
    (cond
      ((atom? x) #f )
      ((null? x) #f )
      ((null? (cdr x)) #f )
      ((null? (cdr (cdr x))) #t )
      (else #f ))))

(define shujjle
  (λ (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shujjle (revpair pora)))
      (else (build (first pora)
                    (shujjle (second pora) ))))))

(define build
  (λ (sl s2 )
    (cond
      (else (cons sl
                   (cons s2 null))))))

(define find-the-row
  (λ (target graph)
    (cond [(empty? graph)  
           null]
          [(equal? target (caar graph)) 
           (cons (cadar graph)(find-the-row target (cdr graph)))]
          [else
           (find-the-row target (cdr graph))]
          )
    ))

(define find-past-helper
  (λ (target graph)
    (cond
      ((empty? target)
       null)
      ((atom? target)
       (find-past-helper (find-the-row target graph)
                         graph))
      (else (cons (car target)
                  (cons (find-past-helper (car target) graph)
                        (find-past-helper (cdr target) graph)))))))
(define Dag
  '(
    (M (F K))
    (J (F H))
    (K (B H I))
    (H (C D E))
    (L (D I))
    (F (B C))
    (I (E))
    (B (G))
    (C (G))
    (D (G))
    (E (G))
    )
  )
(define revpair
  (λ (pair)
    (list (second pair) (first pair))))

(define magic
  (λ (graph)
    (if (null? graph)
        null
        (cons (pair-helper (car graph))
              (magic (cdr graph))))))

(define pair-helper
  (λ (pair)
    (if (null? pair)
        null
        (row-helper (cadr pair) (car pair)))))

(define row-helper
  (λ (row ID)
    (if (null? row)
        null
        (cons (cons (car row)
                    (list (list ID)))
              (row-helper (cdr row) ID)))))

(define collector
  (λ (list)
    (let find ((result '())
               (list list))
      (if (null? list)
          result
          (let ((next (car list))
                (rest (cdr list)))
            (if (list? next)
                (find result (append next rest))
                (find (if (memq next result)
                          result
                          (cons next result))
                      rest)))))))

(define find-past
  (λ (target graph)
    (collector (find-past-helper target graph))))

(define find-future
  (λ (target graph)
    (find-past target (append* (magic graph)))))


(define graph-set
  (λ (graph)
    (flatten graph)))

(define find-anticone
  (λ (target graph)
    (remove* (find-future target graph)
               (remove* (find-past target graph) (collector graph)))))

(define rember-cps
  (λ (remove set col)
    (cond
      ((null? set)
       (col null null))
      ((equal? (car set) remove)
       (rember-cps remove
                   (cdr set)
                   (λ (newlat seen)
                     (col newlat
                           (cons (car set) seen)))))
      (else
       (rember-cps remove
                   (cdr set)
                   (λ (newlat seen)
                     (col (cons (car set) newlat)
                          seen)))))))

(define Y
  (λ (x y)
    (length x)))

(define intersect
  (λ (set1 set2)
    (cond
      ((null? set1 ) null)
      ((member? (car set1 ) set2)
       (cons (car set1 )
              (intersect (cdr set1 ) set2)))
      (else (intersect (cdr set1 ) set2)))))


(define a-part-of
  (λ (target list)
    (cond
      ((null? list) #f )
      ((atom? (car list))
       (or (equal? (car list) target)
           (a-part-of target (cdr list))))
      (else (or (a-part-of target (car list))
                (a-part-of target (cdr list)))))))

(define find-tips-helper
  (λ (graph  x)
  (cond ((null? x)
            null) 
         ((null? (find-future (caar x) graph))
            (cons (caar x) (find-tips-helper graph (cdr x))))
         (else
            (find-tips-helper graph (cdr x))
          ))))

(define Ori Dag)

(define find-mtip-helper
  (λ (tips graph)
    (cond ((null? tips)
           tips)
          ((< (caar (past-list graph))
              (caadr (past-list graph)))
           (find-mtip-helper (cdr tips) graph))
          (else
            (find-mtip-helper (remove (cadr tips) tips) graph)))))

(define find-mtip
  (λ (graph)

    (find-mtip-helper (find-tips-helper graph graph) graph)))


(define rember-list-helper
  (λ ( a lat)
    (cond
      (( null? lat) null)
      ((equal? (caar lat) a)
       (rember-list-helper a ( cdr lat)))
      (else ( cons ( car lat)
                   (rember-list-helper a
                                ( cdr lat)))))))
(define rember-list
  (λ (target origin)
    (cond
      ((null? target )
           origin)
      (else
       ( rember-list (cdr target) (rember-list-helper (car target ) origin))))))
          

(define member?
  (λ (a lat)
    (cond
      ((null? lat) #f )
      (else (or (equal? (caar lat) a)
                (member? a (cdr lat)))))))



(define find-blue-chain
  (λ (graph start)
    (if (null? graph)
        null
       (cons (find-mtipp  (rember-list (find-tips-helper graph graph) graph))
                          (find-blue-chain (cdr graph) start)))))

(define past-list
  (λ (graph)
    (if (null? graph)
        null
        (cons (cons (length (find-past (caar graph) graph))
                 (cons  (caar graph) null))
                    (past-list (cdr graph))))))
(define find-mtipp
  (λ (graph)
   (if (null? graph)
       null
     (argmax car (past-list graph)))
    ))

(define value
  (λ (x)
    (car x)))

"
Dag
(find-tips-helper Dag Dag)
(rember-list (find-tips-helper Dag Dag) Dag)
(rember-list (find-tips-helper Dag Dag) Dag)
(magic (rember-list (find-tips-helper Dag Dag) Dag))
(rember-list (find-tips-helper Dag Dag) Dag)
(find-tips-helper (remove-pair (find-tips-helper Dag Dag) Dag) Dag)
(find-future 'K (rember-list (find-tips-helper Dag Dag) Dag))
(argmax add1 '(3 2))
(find-past 'H Dag)
(find-future 'H Dag)
(find-anticone 'H Dag)
(caadr Dag)
(find-tips Dag)
"

"
(magic Dag)
(append* (magic Dag))
(remove-pair (list 'H 'I) Dag)
Dag
(remove-pair (find-tips-helper Dag Dag) Dag)

(find-blue-chain Dag 'M)
(argmax add1 '(3 2))
(find-past 'H Dag)
(find-future 'H Dag)
(find-anticone 'H Dag)
(caadr Dag)
(length (find-past (find-tips-helper Dag Dag) Dag))

(find-mtip Dag)


(find-blue-chain Dag 'M)
(find-mtip (rember-list (find-tips-helper Dag Dag) Dag))

(past-list Dag)
(argmax value '((3 A)(2 B)))

(find-mtipp Dag)
"
