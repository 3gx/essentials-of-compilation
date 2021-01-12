#lang racket

(require racket/struct)

;(struct Int (value) #:prefab)
;(struct Prim (op args) #:prefab)

(define AST-output-syntax (make-parameter 'abstract-syntax))
(define (make-recur port mode)
  (case mode
    [(#t) write]
    [(#f) display]
    [else (lambda (p port) (print p port mode))]
    ))




(struct Int (value) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let ([csp (make-constructor-style-printer
                 (lambda (obj) 'Int)
                 (lambda (obj) (list (Int-value obj))))])
       (lambda  (ast port mode)
         (cond [(eq? (AST-output-syntax) 'concrete-syntax)
                (let ([recur (make-recur port mode)])
                  (match ast
                    [(Int n)
                     (recur n port)]))]
               [(eq? (AST-output-syntax) 'abstract-syntax)
                (csp ast port mode)]
               ))))])

(struct Prim (op arg*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let ([csp (make-constructor-style-printer
                 (lambda (obj) 'Prim)
                 (lambda (obj) (list (Prim-op obj) (Prim-arg* obj))))])
       (lambda (ast port mode)
         (cond [(eq? (AST-output-syntax) 'concrete-syntax)
                (let ([recur (make-recur port mode)])
                  (match ast
                    [(Prim op arg*)
                     (write-string "(" port)
                     (write-string (symbol->string op) port)
                     (for ([arg arg*])
                       (write-string " " port)
                       (recur arg port))
                     (write-string ")" port)
                     ]))]
               [(eq? (AST-output-syntax) 'abstract-syntax)
                (csp ast port mode)]))))])



(define eight (Int 8))
(printf "value: ~a\n" eight)
(define neg-eight (Prim '- (list eight)))
(define rd (Prim 'read '()))
(define ast1.1 (Prim '+ (list rd neg-eight)))


(printf "expr: ~a\n" ast1.1)
