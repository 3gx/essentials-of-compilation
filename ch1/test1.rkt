#lang racket

(require racket/struct)

(struct Int (value) #:transparent)

(define intval8 (Int 8))
(printf "value: ~a\n" intval8)
