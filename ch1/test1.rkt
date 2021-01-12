#lang racket

(require racket/struct)

(struct Int (value) #:prefab)

(define eight (Int 8))
(printf "value: ~a\n" eight)

(struct Prim (op args) #:prefab)
(define neg-eight (Prim '- (list eight)))
(define rd (Prim 'read '()))
(define ast1.1 (Prim '+ (list rd neg-eight)))


(printf "expr: ~a\n" ast1.1)
