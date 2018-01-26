#lang racket

; Exports.
(provide pn-calc (struct-out branch) (struct-out leaf) blt-fold blt-foldl)


; Question 1.
(define (pn-calc lst)
  (cond
    [(not (isValid lst)) 'syntax-error]
    [else (let ([result (evaluate lst)])
             (cond
               [(eq? (cdr result) null) (car result)]
               [else 'syntax-error]))])
)
; helper function for Question 1
(define (evaluate lst)
  ; match the first element in the list into different situations
  (let([x (list-ref lst 0)])
    (cond
      [(number? x) (cons x (list-tail lst 1))]
      ; if its + binary operator, then perform + on the followting 2 expressions
      ; and construct a pair from the result and the rrest of the list
      [(eq? x '-) (cons (- (car (evaluate (list-tail lst 1))) (car (evaluate (cdr (evaluate (list-tail lst 1))))))
                        (cdr (evaluate (cdr (evaluate (list-tail lst 1))))))]
      ; or perform -
      [(eq? x '+) (cons (+ (car (evaluate (list-tail lst 1))) (car (evaluate (cdr (evaluate (list-tail lst 1))))))
                        (cdr (evaluate (cdr (evaluate (list-tail lst 1))))))]
      ; or *
      [(eq? x '*) (cons (* (car (evaluate (list-tail lst 1))) (car (evaluate (cdr (evaluate (list-tail lst 1))))))
                        (cdr (evaluate (cdr (evaluate (list-tail lst 1))))))]
      ; or unary operator
      [(eq? x 'neg) (cons (- 0 (car (evaluate (list-tail lst 1))))
                          (cdr (evaluate (list-tail lst 1))))]))
)

(define (isValid lst)
  ; check if the lenght of the lst is valid for a Polish expression
  ; by checking total number of numbers/different operators/non-number elements
  (define countNumber (length (filter number? lst)))
  (define countBiOperator (length (filter-map (lambda (x) (or (eq? x '+) (eq? x '-) (eq? x '*))) lst)))
  (define countUOperator (length (filter-map (lambda (x) (eq? x 'neg)) lst)))
  (define countInvalid (- (length lst) countNumber countBiOperator countUOperator))
  (cond
    [(> countInvalid 0) #f]
    [(not (= countNumber (+ 1 countBiOperator))) #f]
    [(and (> countUOperator 0) (< countNumber 1)) #f]
    [else #t])
)
; Binary leafy tree.
(struct branch (left right) #:transparent)
(struct leaf (datum) #:transparent)
; The #:transparent enables printing out these records at the REPL.

(define alberts-tree
  (branch
    (branch
      (leaf 2)
      (branch (leaf 7) (leaf 8)))
    (branch (leaf 6) (leaf 3))))


; Question 2.
(define (blt-fold binop f tree)
  (cond
    ; its a leaf
    [(leaf? tree) (f (leaf-datum tree))]
    ; its a branch
    [else (binop (blt-fold binop f (branch-left tree))
                 (blt-fold binop f (branch-right tree)))])
)


; Question 3.
(define (blt-foldl a0 binop tree)
  (cond
    ; its a leaf
    [(leaf? tree) (binop a0 (leaf-datum tree))]
    ; its a branch
    [else
       (let ([a1 (blt-foldl a0 binop (branch-left tree))])
            (blt-foldl a1 binop (branch-right tree)))])
  )



;  tests Q1
;(print "test for Q1 on '(neg 3 neg 5)  ")
;(define t1 '(neg 3 neg 5))
;(pn-calc t1)

; test Q2
;(blt-fold - log alberts-tree)

; test Q3
;(blt-foldl 20 - alberts-tree)