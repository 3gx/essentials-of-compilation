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


(define (print-info info port mode)
  (let ([recur (make-recur port mode)])
    (for ([(label data) (in-dict info)])
      (match label
        ['locals-types
         (write-string "locals-types:" port)
         (newline port)
         (cond [(dict? data)
                (write-string "    " port)
                (for ([(var type) (in-dict data)])
                  (write-string (symbol->string var) port)
                  (write-string " : " port)
                  (recur type port)
                  (write-string ", " port)
                  )
                (newline port)]
               [else
                (recur data port)
                (newline port)])]
        [else
         (write-string (symbol->string label) port)
         (write-string ":" port)
         (newline port)
         (recur data port)
         (newline port)
         ]))))


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


(struct Program (info body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let ([csp (make-constructor-style-printer
                 (lambda (obj) 'Program)
                 (lambda (obj) (list (Program-info obj) (Program-body obj))))])
       (lambda (ast port mode)
         (cond [(eq? (AST-output-syntax) 'concrete-syntax)
                (let ([recur (make-recur port mode)])
                  (match ast
                    [(Program info body)
                     (write-string "program:" port)
                     (newline port)
                     (print-info info port mode)
                     (cond [(list? body)
                            (for ([def body])
                              (recur def port)
                              (newline port))]
                           [else
                            (recur body port)])]))]
               [(eq? (AST-output-syntax) 'abstract-syntax)
                (csp ast port mode)]
                ))))])


(define eight (Int 8))
(printf "value: ~a\n" eight)
(define neg-eight (Prim '- (list eight)))
(define rd (Prim 'read '()))
(define ast1.1 (Prim '+ (list rd neg-eight)))


(printf "expr: ~a\n" ast1.1)

(match ast1.1
  [(Prim op (list child1 child2))
    (println (format "op: ~a" op))])

(define (leaf? arith)
  (match arith
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list e1)) #f]
    [(Prim '+ (list e1 e2 )) #f]))

(leaf? (Prim 'read '()))
(leaf? (Prim '- (list (Int 8))))
(leaf? (Int 8))

(define (exp? ast)
  (match ast
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list e)) (exp? e)]
    [(Prim '+ (list e1 e2)) (and (exp? e1) (exp? e2))]
    [else #f]))

(define (Rint? ast)
  (match ast
    [(Program '() e) (exp? e)]
    [else #f]))

(Rint? (Program '() ast1.1))
(Rint? (Program '()
                (Prim '- (list (Prim 'read '())
                      (Prim '+ (list (Int 8)))))))


(define (Rint-incorrect? ast) (match ast
                       [(Int n) #t]
                       [(Prim 'read '()) #t]
                       [(Prim '- (list e)) (Rint-incorrect? e)]
                       [(Prim '+ (list e1 e2)) (and (Rint-incorrect? e1)
                                                    (Rint-incorrect? e2))] [(Program '() e) (Rint? e)]
                       [else #f]))

(Rint-incorrect? (Program '() (Program '() (Int 3))))
(Rint? (Program '() (Program '() (Int 3))))
