; top-level-eval evaluates a form in the global environment

(define *prim-proc-names* '(+ - * add1 sub1 cons = / not zero? car cdr list null?
              assq eq? equal? atom? length list->vector list? pair?
              procedure? vector->list vector vector? number? symbol?
              caar cadr cadar >= make-vector vector-ref set-car! set-cdr!))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

(define global-env init-env)
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    ; (display form)
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      	[lit-exp (datum) datum]
      	[var-exp (id)
				    (apply-env env id; look up its value.
      	  	  (lambda (x) x) ; procedure to call if id is in the environment 
           	  (lambda ()
                (apply-env global-env
                  id
                  (lambda (x) x)
                  (lambda ()
                    (eopl:error 'apply-env ; procedure to call if id not in env
		                  "variable not found in environment: ~s"
			   	id)))))]
      	[app-exp (rator rands)
        	(let ([proc-value (eval-exp rator env)]
             	[args (eval-rands rands env)])
          		(apply-proc proc-value args))]

      	[if-exp (condition body)
            (if (eval-exp condition env) (eval-exp body env))]
      	[if-else-exp (condition body1 body2)
            (if (eval-exp condition env) (eval-exp body1 env) (eval-exp body2 env))]

        [let-exp (vars vals body)
            (eval-in-order body (extend-env vars (eval-rands vals env) env))]

        [lambda-exp (id body) 
            (closure id body env)]
        
      	[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-in-order
      (lambda (body env)
            (cond
              [(null? (cdr body)) (eval-exp (car body) env)]
              [else (begin (eval-exp (car body) env) (eval-in-order (cdr body) env))])))

(define eval-rands
  (lambda (rands env)
    (map (lambda (expr) (eval-exp expr env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases

      [closure (vars bodies env) (eval-in-order bodies (extend-env vars args env))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))



; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(add1) (+ (car args) 1)]
      [(sub1) (- (car args) 1)]
      [(cons) (cons (car args) (cadr args))]
      [(=) (= (car args) (cadr args))]
      [(/) (apply / args)]
      [(not) (not (car args))]
      [(zero?) (zero? (car args))]

      [(car) (car (car args))]
      [(cdr) (cdr (car args))]
      [(list) args]
      [(null?) (null? (car args))]
      [(assq) (assq (car args) (cadr args))]
      [(eq?) (eq? (car args) (cadr args))]
      [(equal?) (equal? (car args) (cadr args))]
      [(atom?) (atom? (car args))]
      [(length) (length (car args))]
      [(list->vector) (list->vector (car args))]
      [(list?) (list? (car args))]
      [(pair?) (pair? (car args))]
      [(procedure?) (proc-val? (car args))]			; TODO: Fix this. fails just this TC for primitives.
      [(vector->list) (vector->list (car args))]
      [(vector) (vector (car args))]
      [(vector?) (vector? (car args))]
      [(number?) (number? (car args))]
      [(symbol?) (symbol? (car args))]
      [(caar) (caar (car args))]
      [(cadr) (cadr (car args))]
      [(cadar) (cadar (car args))]
      [(>=) (>= (car args) (cadr args))]
	  
	    [(make-vector) (make-vector (car args))]
	    [(vector-ref) (vector-ref (car args) (cadr args))]
	    [(set-car!) (set-car! (car args) (cadr args))]
	    [(set-cdr!) (set-cdr! (car args) (cadr args))]

      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))










