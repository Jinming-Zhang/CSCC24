#lang racket

; Exports.
(provide pn-calc (struct-out branch) (struct-out leaf) blt-fold blt-foldl)


; Question 1.
(define (pn-calc lst)
  'TO-DO)


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
  'TO-DO)


; Question 3.
(define (blt-foldl a0 binop tree)
  'TO-DO)
