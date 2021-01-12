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

(define prog1
  (Program '() (Prim '+ (list (Int 10) (Int 32)))))
(println (format "~a" prog1))
(println prog1)

(define (interp-exp e)
  (match e
    [(Int n) n]
    [(Prim 'read '())
     (define r (read))
     (cond [(fixnum? r) r]
           [else (error 'interp-exp "read expected an integer" r)])]
    [(Prim '- (list e))
     (define v (interp-exp e))
     (- 0 v)]
    [(Prim '+ (list e1 e2))
     (define v1 (interp-exp e1))
     (define v2 (interp-exp e2))
     (+ v1 v2)]))

(define (interp-Rint p)
  (match p
    [(Program '() e) (interp-exp e)]))

(interp-Rint prog1)
;(interp-Rint ast1.1)

(define prog2
  (Program '() (Prim '+ (list (Prim 'read '()) (Int 32)))))
(println prog2)
;(interp-Rint prog2)

;(interp-Rint (Program '() ast1.1))

(define (pe-neg r)
  (match r
    [(Int n) (Int (- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Rint p)
  (match p
    [(Program '() e) (Program '() (pe-exp e))]))

(define prog3 (Program '() (Prim '+ (list (Prim 'read '())
                             (Prim '- (list (Prim '+ (list (Int 5) (Int
                                                                     3)))))))))
(define pe-prog3 (pe-Rint prog3))
(println pe-prog3)

;(interp-Rint pe-prog3)

(define assert
  (lambda (msg b)
    (if (not b)
	(begin
	  (display "ERROR: ")
	  (display msg)
	  (newline))
	(void))))

(define (write-type ty port)
  (match ty
    [`(Vector ,tys ...)
     (write-string "(Vector" port)
     (for ([ty tys])
       (write-string " " port)
       (write-type ty port))
     (write-string ")" port)]
    [`(PVector ,tys ...)
     (write-string "(PVector" port)
     (for ([ty tys])
       (write-string " " port)
       (write-type ty port))
     (write-string ")" port)]
    [`(,ts ... -> ,rt)
     (write-string "(" port)
     (for ([t ts])
       (write-type t port)
       (write-string " " port))
     (write-string "-> " port)
     (write-type rt port)
     (write-string ")" port)]
    [(? symbol?)
     (write-string (symbol->string ty) port)]
    ))


(define (write-params param* port)
  (define fst #t)
  (for ([param param*])
    (match param
      [(? symbol?)
       (write-string (symbol->string param) port)]
      [`(,x : ,t)
       (if fst (set! fst #f) (write-string " " port))
       (write-string "[" port)
       (write-string (symbol->string x) port)
       (write-string " : " port)
       (write-type t port)
       (write-string "]" port)])))

(define (newline-and-indent port col)
  (let ([lead (if col (make-string col #\space) "")])
    (newline port)
    (write-string lead port)
    ))



(struct Def (name param* rty info body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let ([csp (make-constructor-style-printer
                 (lambda (obj) 'Def)
                 (lambda (obj) (list (Def-name obj) (Def-param* obj)
                                     (Def-rty obj) (Def-info obj)
                                     (Def-body obj))))])
       (lambda (ast port mode)
         (cond [(eq? (AST-output-syntax) 'concrete-syntax)
                (let ([recur (make-recur port mode)])
                  (match ast
                    [(Def name ps rty info body)
                     (let-values ([(line col pos) (port-next-location port)])
                       (write-string "(define (" port)
                       (write-string (symbol->string name) port)
                       (if (< 0 (length ps)) (write-string " " port) (void))
                       (write-params ps port)
                       (write-string ") " port)
                       (write-string ":" port)
                       (write-string " " port)
                       (write-type rty port)
                       (newline-and-indent port col)
                       (write-string "   " port)
                       (cond [(list? body)
                              (for ([block body])
                                (cond [(pair? block)
                                       (write-string (symbol->string (car block)) port)
                                       (write-string ":" port)
                                       (newline-and-indent port col)
                                       (write-string "      " port)
                                       (recur (cdr block) port)
                                       (newline-and-indent port col)
                                       (write-string "   " port)]
                                      [else
                                       (recur block port)
                                       (newline-and-indent port col)
                                       (write-string "   " port)]))]
                             [else
                              (recur body port)])
                       (newline-and-indent port col)
                       (write-string ")" port)
                       )]))]
               [(eq? (AST-output-syntax) 'abstract-syntax)
                (csp ast port mode)]
               ))))])

(struct Apply (fun arg*) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let ([csp (make-constructor-style-printer
                 (lambda (obj) 'Apply)
                 (lambda (obj) (list (Apply-fun obj) (Apply-arg* obj))))])
       (lambda (ast port mode)
         (cond [(eq? (AST-output-syntax) 'concrete-syntax)
                (let ([recur (make-recur port mode)])
                  (match ast
                    [(Apply fun arg*)
                     (write-string "(" port)
                     (recur fun port)
                     (for ([arg arg*])
                       (write-string " " port)
                       (recur arg port))
                     (write-string ")" port)
                     ]))]
               [(eq? (AST-output-syntax) 'abstract-syntax)
                (csp ast port mode)]
               ))))])

(define src-primitives
  '(read + - eq? < <= > >= and or not
         vector vector-ref vector-set! vector-length
         procedure-arity
         boolean? integer? vector? procedure? void?
         any-vector-ref any-vector-set! any-vector-length))

(struct AssignedFree (var)
  #:transparent #:property prop:custom-print-quotable 'never)

(define (unsymbolic e)
  (match e
    [(AssignedFree x) x]
    [else e]))


(struct Void () #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let ([csp (make-constructor-style-printer
                 (lambda (obj) 'Void)
                 (lambda (obj) (list)))])
       (lambda (ast port mode)
         (cond [(eq? (AST-output-syntax) 'concrete-syntax)
                (match ast
                  [(Void)
                   (write-string "(void)" port)])]
               [(eq? (AST-output-syntax) 'abstract-syntax)
                (csp ast port mode)]
               ))))])

(struct If (cnd thn els) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let ([csp (make-constructor-style-printer
                 (lambda (obj) 'If)
                 (lambda (obj) (list (If-cnd obj) (If-thn obj) (If-els obj))))])
       (lambda (ast port mode)
         (cond [(eq? (AST-output-syntax) 'concrete-syntax)
                (let ([recur (make-recur port mode)])
                  (match ast
                    [(If cnd thn els)
                     (let-values ([(line col pos) (port-next-location port)])
                       (write-string "(if" port)
                       (write-string " " port)
                       (recur cnd port)
                       (newline-and-indent port col)
                       (write-string "   " port) ;; indent 
                       (recur thn port)
                       (newline-and-indent port col)
                       (write-string "   " port) ;; indent 
                       (recur els port)
                       (write-string ")" port)
                       (newline-and-indent port col)
                       )]))]
               [(eq? (AST-output-syntax) 'abstract-syntax)
                (csp ast port mode)]))))])


(struct Let (var rhs body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let ([csp (make-constructor-style-printer
                 (lambda (obj) 'Let)
                 (lambda (obj) (list (Let-var obj) (Let-rhs obj)
                                     (Let-body obj))))])
       (lambda ( ast port mode)
         (cond [(eq? (AST-output-syntax) 'concrete-syntax)
                (let ([recur (make-recur port mode)])
                  (match ast
                    [(Let x rhs body)
                     (let-values ([(line col pos) (port-next-location port)])
                       (write-string "(let ([" port)
                       (write-string (symbol->string (unsymbolic x)) port)
                       (write-string " " port)
                       (recur rhs port)
                       (write-string "])" port)
                       (newline-and-indent port col)
                       (write-string "   " port) ;; indent body
                       (recur body port)
                       (write-string ")" port)
                       ;(newline-and-indent port col)
                       )]))]
               [(eq? (AST-output-syntax) 'abstract-syntax)
                (csp ast port mode)]
               ))))])

(struct Lambda (param* rty body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let ([csp (make-constructor-style-printer
                 (lambda (obj) 'Lambda)
                 (lambda (obj) (list (Lambda-param* obj) (Lambda-rty obj)
                                     (Lambda-body obj))))])
       (lambda (ast port mode)
         (cond [(eq? (AST-output-syntax) 'concrete-syntax)
                (let ([recur (make-recur port mode)])
                  (match ast
                    [(Lambda ps rty body)
                     (let-values ([(line col pos) (port-next-location port)])
                     (write-string "(lambda: (" port)
                     (write-params ps port)
                     (write-string ") " port)
                     (write-string ":" port)
                     (write-string " " port)
                     (write-type rty port)
                     (newline-and-indent port col)
                     (write-string "   " port)
                     (recur body port)
                     (write-string ")" port)
                     (newline-and-indent port col)
                     )]))]
               [(eq? (AST-output-syntax) 'abstract-syntax)
                (csp ast port mode)]
               ))))])

(define (parse-exp e)
  (match e
;    [(? symbol?) (Var e)]
    [(? fixnum?) (Int e)]
;    [(? boolean?) (Bool e)]
    [`(void) (Void)]
    [`(let ([,x ,rhs]) ,body) (Let x (parse-exp rhs) (parse-exp body))]
    [`(if ,cnd ,thn ,els) (If (parse-exp cnd) (parse-exp thn) (parse-exp els))]
    [`(lambda: ,ps : ,rt ,body)
     (Lambda ps rt (parse-exp body))]
    [`(lambda: ,ps ,body)
     (Lambda ps 'Any (parse-exp body))]
    [`(lambda ,ps ,body) ;; dynamically typed lambda
     (Lambda ps 'Any (parse-exp body))]
;    [`(project ,e ,t)
;     (Project (parse-exp e) t)]
;    [`(inject ,e ,t)
;     (Inject (parse-exp e) t)]
;    [`(while ,cnd ,body)
;     (WhileLoop (parse-exp cnd) (parse-exp body))]
;    [`(set! ,x ,rhs)
;     (SetBang x (parse-exp rhs))]
;    [`(begin ,es ... ,e)
;     (Begin (for/list ([e es]) (parse-exp e)) (parse-exp e))]
;    [`(has-type ,e ,t)
;     (HasType (parse-exp e) t)]
    [`(,op ,es ...)
     #:when (set-member? src-primitives op)
     (Prim op (for/list ([e es]) (parse-exp e)))]
;    [`(,e ,es ...)
;     (Apply (parse-exp e) (for/list ([e es]) (parse-exp e)))]
    ))


(define (parse-def d)
  (match d
    [`(define (,f ,ps ...) : ,rty ,body)
     (Def f ps rty '() (parse-exp body))]
    [`(define (,f ,xs ...) ,body) ;; dynamically typed definition
     (Def f xs 'Any '() (parse-exp body))]
;    [`(: ,name ,type)
;     (Decl name type)]
    ))


(define (parse-program p)
  (match p
    [`(program ,info ,body)
     (Program info (parse-exp body))]
    [`(program ,info ,def* ... ,body)
     (ProgramDefsExp info
                  (for/list ([d def*]) (parse-def d))
                  (parse-exp body))]
    ))



(struct ProgramDefsExp (info def* body) #:transparent #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc
     (let ([csp (make-constructor-style-printer
                 (lambda (obj) 'ProgramDefsExp)
                 (lambda (obj) (list (ProgramDefsExp-info obj)
                                     (ProgramDefsExp-def* obj)
                                     (ProgramDefsExp-body obj))))])
       (lambda (ast port mode)
         (cond [(eq? (AST-output-syntax) 'concrete-syntax)
                (let ([recur (make-recur port mode)])
                  (match ast
                    [(ProgramDefsExp info def* body)
                     (write-string "functions:" port)
                     (newline port)
                     (for ([def def*]) (recur def port)(newline port))
                     (write-string "program:" port)
                     (newline port)
                     (recur body port)
                     ]))]
               [(eq? (AST-output-syntax) 'abstract-syntax)
                (csp ast port mode)]
               ))))])


(define (test-pe p)
  (println (interp-Rint p))
  (assert "testing pe-Rint"
  (equal? (interp-Rint p) (interp-Rint (pe-Rint p)))))


(test-pe (parse-program `(program () (+ 10 (- (+ 5 3))))))
(test-pe (parse-program `(program () (+ 1 (+ 3 1)))))
(test-pe (parse-program `(program () (- (+ 3 (- 5))))))
