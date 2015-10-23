; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
      (cond
        [(symbol? datum) (var-exp datum)]
        [(pair? datum)
          (cond
              [(not (list? datum)) (eopl:error 'parse-exp "The expression ~s is not a list" datum)]
              [(equal? (car datum) 'lambda) (parse-lambda datum)]
              [(equal? (car datum) 'while) (parse-while datum)]
              [(equal? (car datum) 'if) (parse-if datum)]
              [(equal? (car datum) 'let) (parse-let datum)]
              [(equal? (car datum) 'let*) (parse-let* datum)]
              [(equal? (car datum) 'letrec) (parse-letrec datum)]
              [(equal? (car datum) 'set!) (parse-set! datum)]
              [(equal? (car datum) 'quote) (lit-exp (2nd datum))]
              [else (app-exp (parse-exp (1st datum))
                (map parse-exp (cdr datum)))])]
        [(lit2? datum) (lit-exp datum)]
    [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define parse-while
	(lambda (datum)
		(while-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))))

(define parse-lambda
  (lambda (datum)
    (cond
      [(< (length datum) 3) (eopl:error 'parse-exp "length error ~s" datum)]

      [(symbol? (2nd datum)) (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
      [(andmap symbol? (2nd datum)) (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
      ; [(or (null? (2nd datum)) (symbol? (2nd datum)) (pair? (2nd datum))) (lambda-exp (2nd datum) (map parse-exp (cddr datum)))] 
      [else (eopl:error 'parse-exp "invalid arguments for lambda ~s" (2nd datum))])))

(define parse-if
  (lambda (datum)
    (cond
      [(= 3 (length datum)) (if-exp (parse-exp (2nd datum) (parse-exp (3rd datum))))]
      [(= 4 (length datum)) (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (cadddr datum)))]
      [else (eopl:error 'parse-exp "if statement is atleast 3 ~s" datum)])))


(define parse-let
  (lambda (datum)
    (cond
      [(< (length datum) 3) (eopl:error 'parse-exp "invalid length ~s" datum)]
      [(= 3 (length datum)) (if (let-helper (2nd datum)) 
                        (let-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))
                        (eopl:error 'parse-exp "invalid syntax ~s" datum))]
      [else (cond
            [(not (symbol? (2nd datum))) (if (let-helper (2nd datum))
                            (let-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum))) 
                            (eopl:error 'parse-exp "name of named let is not a name ~s" datum))]
            [(let-helper (3rd datum)) (named-let-exp (2nd datum) (map (lambda (x) (list (1st x) (parse-exp (2nd x)))) (3rd datum)) (map parse-exp (cdddr datum)))]
            [else (eopl:error 'parse-exp "syntax error in named let ~s" datum)])])))

(define parse-let*
  (lambda (datum)
    (cond
      [(> 3 (length datum)) (eopl:error 'parse-exp "invalid length of let* ~s" datum)]
      [(let-helper (2nd datum)) (let*-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))]
      [else (eopl:error 'parse-exp "invalid syntax of let* ~s" datum)])))


(define parse-letrec
  (lambda(datum)
    (cond
      [(> 3 (length datum)) (eopl:error 'parse-exp "invalid length of letrec ~s" datum)]
      [(let-helper (2nd datum)) (letrec-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))]
      [else (eopl:error 'parse-exp "invalid syntax of letrec ~s" datum)])))


(define parse-set!
  (lambda (datum)
    (cond
      [(= (length datum) 3) (set!-exp (2nd datum) (parse-exp (3rd datum)))]
      [else (eopl:error 'parse-exp "invalid syntaxt of set! ~s" datum)])))



(define let-helper
  (lambda (datum)
    (cond
      [(null? datum) #t]
      [(list? datum) (andmap (lambda (e) (cond [(not (list? e)) #f] [(not (symbol? (car e))) #f] [(not (= (length e) 2)) #f] [else #t])) datum)]
      [else #f])))


(define lambda-helper
  (lambda (x)
    (or (null? x) (symbol? x) (pair? x))))


(define lit2? 
  (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))


(define unparse-exp
  (lambda (datum)
    (cases expression datum
      [var-exp (var) var]
	  [while-exp (test-exp bodies) (cons 'while (cons (unparse-exp test-exp) (map unparse-exp bodies)))]
      [lambda-exp (id body) (cons* 'lambda id (map unparse-exp body))]
      [if-exp (condition body) (list 'if (unparse-exp condition) (unparse-exp body))]
      [if-else-exp (condition body1 body2) (list 'if (unparse-exp condition) (unparse-exp body1) (unparse-exp body2))]
      [let-exp (vars vals body) (cons* 'let (unparse-let vars vals '()) (map unparse-exp body))]
      [let*-exp (vars vals body) (cons* 'let* (unparse-let vars vals '()) (map unparse-exp body))]
      [letrec-exp (vars vals body) (cons* 'letrec (unparse-let vars vals '()) (map unparse-exp body))]
      [named-let-exp (id vars vals body) (cons* 'let id (unparse-let vars vals '()) (map unparse-exp body))]
      [set!-exp (id body) (cons* 'set! id (unparse-exp body))]
      [lit-exp (id) id]
      [vec-exp (id) id]
      [app-exp (rator rand)
        (cons*
          (unparse-exp rator) (map unparse-exp rand))])))



(define unparse-let
  (lambda (vars vals ans)
    (cond
      [(null? vars) (reverse ans)]
      [else (unparse-let (cdr vars) (cdr vals) (cons (list (car vars) (unparse-exp (car vals))) ans))])))



