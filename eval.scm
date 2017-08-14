(define (eval exp env)
  (cond ((self-evaluating? exp) exp) ;自求值表达式返回其自身，如数字和字符串
        ((variable? exp) (lookup-variable-value exp env)) ;变量，在环境中查找其值
        ((quoted? exp) (text-of-qutation exp))  ;返回被引表达式
        ((assignment? exp) (eval-assignment exp env))  ;赋值,求值其所赋的值
        ((definition? exp) (eval-definition exp env))  
        ((if? exp) (eval-if exp env))
		((let? exp) (eval-let exp env))
		((let*? exp) (eval-let* exp env))
		((case? exp) (eval-case exp env))
		((letrec? exp) (eval-letrec exp env))
		((delay? exp) (eval-delay exp env))
		((force? exp) (eval-force exp env))
		((promise? exp) (error "[promise] is not applicable")(driver-loop))
		((and? exp)(eval-and exp env))
		((or? exp)(eval-or exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))  
        ((cond? exp) (eval (cond->if exp) env))  ;将条件表达式转为if表达式
        ((application? exp)  ;对一个过程，先求其子过程
         (apply (eval (operator exp) env)  ;apply函数将实际参数应用到函数上 
                (list-of-values (operands exp) env)))  ;求出参数的值
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define apply-in-underlying-scheme apply)

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)  ;如果是基本过程，直接应用基本过程到参数上
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)  ;如果是复合过程
         (eval-sequence  ;顺序求值,包装为一个集装箱，有
          (procedure-body procedure)  ;过程的体
          (extend-environment  ;过程的环境，将形式参数约束为实际参数，加上过程携带的基本环境
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)  ;生成实际参数表
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)  ;求值if
  (if (true? (eval (if-predicate exp) env))  ;求值if的谓词部分
      (eval (if-consequent exp) env)  
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)  ;求值序列
  (cond ((last-exp? exps) (eval (first-exp exps) env))  ;返回最后一个表达式的值
        (else (eval (first-exp exps) env)  ;如果不是最后一个表达式，则递归求值
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)  ;求值赋值
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)  ;求值定义
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;表达式的表示

;自求值表达式只有数字和字符串
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
		((boolean? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

;(quote <text-of-quotation>)
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-qutation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;(set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;(define <var> <value>)  形式1
;OR
;(define (<var> <parameter 1> ... <parameter n>)  形式2
;   <body>)
;这个只是下面这种形式的语法糖
;(define <var>
;   (lambda (<parameter 1> ... <parameter n>)
;        <body>))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)  ;形式1
      (caadr exp)))   ;形式2
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)   ;形式1
      (make-lambda (cdadr exp)  ;parameters
                   (cddr exp))))  ;body

;lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      '#f))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

 ;let
(define (let? exp)(tagged-list? exp 'let))
(define (eval-let exp env)

	(if(symbol? (cadr exp))
		(begin
			(eval (let->definition exp) env)
			(eval (cons (cadr exp) (map cadr (caddr exp))) env))
		(eval (let->lambda exp) env)
		))
		
(define (let->lambda exp)
    (let ((var-and-exp-list (cadr exp))
          (body (cddr exp)))
        (let ((var-list (map car var-and-exp-list))
              (exp-list (map cadr var-and-exp-list)))
            (cons (make-lambda var-list body) exp-list))))
			
			
(define (let->definition exp)
    (let ((var-and-exp-list (caddr exp))
          (body (cdddr exp)))
        (let ((var-list (map car var-and-exp-list)))
            (list 'define (cadr exp) (make-lambda var-list body)))))
			
;let*
(define (let*? exp)(tagged-list? exp 'let*))
(define (eval-let* exp env)(eval (let*->lets exp) env))
(define (let*->lets exp)
	(let ((args (cadr exp)))
		(if(not (pair? args))
			(if(null?)(list 'let '() (caddr exp)) '#f)
			(make-lets args (caddr exp)))))
(define (make-lets args body)
	(let ((first (car args))(rest (cdr args)))
		(if(null? rest)
			(list 'let (list first) body)
			(list 'let (list first) (make-lets rest body)))))

;letrec
(define (letrec? exp)(tagged-list? exp 'letrec))
(define (eval-letrec exp env)
	(if(null? (cadr exp))
		(begin (newline)(display "no definations in letrec")(newline))
		(make-definations (cadr exp) env))
	(eval (cons 'begin (cddr exp)) env))
(define (make-definations defination-list env)
	(if(null? (cdr defination-list))
		(eval (cons 'define (car defination-list)) env)
		(begin
			(eval (cons 'define (car defination-list)) env)
			(make-definations (cdr defination-list) env))))


;case
(define (case? exp)(tagged-list? exp 'case))
(define (get-key-value exp env)(eval (cadr exp) env))
(define (eval-case exp env)(eval (make-cases (get-key-value exp env) (caddr exp) (cdddr exp) env) env))

(define (make-cases target first-clause rest-clauses env)
	(if(tagged-list? first-clause 'else)
		(cadr first-clause)
		(if(null? rest-clauses)
			(if(and (judge-dup (car first-clause) env) (case-judge-equal (car first-clause) target env))
				(cadr first-clause)
				"no return value")
			(if(and (judge-dup (car first-clause) env) (case-judge-equal (car first-clause) target env))
				(cadr first-clause)
				(make-cases target (car rest-clauses) (cdr rest-clauses) env)))))

(define (judge-dup datum env)
	(if(null? datum)
		#t
		(let iter1((first (eval (car datum) env))(rest (cdr datum)))
			(if (null? rest)
				#t
				(let iter2((f2 first)(r2 rest))
					(if(null? r2)
						(iter1 (eval (car rest) env) (cdr rest))
						(if(eqv? f2 (eval (car r2) env))
							#f
							(iter2 f2 (cdr r2)))))))))
(define (case-judge-equal datum target env)
	(if(null? datum)
		#f
		(let iter((first (eval(car datum) env))(rest (cdr datum)))
			(if(null? rest)
				(if(eqv? first target)
					#t
					#f)
				(if(eqv? first target)
					#t
					(iter (eval (car rest) env) (cdr rest)))))))

;delay
(define (delay? exp)(tagged-list? exp 'delay))
(define (eval-delay exp env)
	(make-promise (cadr exp) env))
(define (make-promise exp env)
	(display "[#promise]")
	;(display (list 'promise (promise-func-closure (lambda() (eval exp env)))))
	(list 'promise (promise-func-closure (lambda() (eval exp env)))))
(define promise-func-closure
       (lambda (proc)
         (let ((already-run? #f)
               (result #f))
           (lambda ()
             (cond ((not already-run?)
                    (set! result (proc))
                    (set! already-run? #t)))
             result))))

;force
(define (force? exp)(tagged-list? exp 'force))
(define (eval-force exp env)
	(let((promise (eval (cadr exp) env)))
		(if(eq? 'promise (car promise))
			((cadr promise))
			(error "is not promise" promise))))

;promise
(define (promise? exp)(tagged-list? exp 'promise))

;and
(define (and? exp)(tagged-list? exp 'and))

(define (eval-and exp env)
	(let ((judge-list (cdr exp)))
		(if (null? judge-list)
			#t
			(let iter ((val (eval (car exp) env))(rest (cdr exp)))
				(if (eq? #f val)
					#f
					(if(null? rest)
						#t
						(iter (eval (car rest) env) (cdr rest))))))))

;or
(define (or? exp)(tagged-list? exp 'or))

(define (eval-or exp env)
	(let ((judge-list (cdr exp)))
		(if (null? judge-list)
			#t
			(let iter ((val (eval (car exp) env))(rest (cdr exp)))
				(if (eq? #t val)
					#t
					(if(null? rest)
						#f
						(iter (eval (car rest) env) (cdr rest))))))))

;begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;cond是if的语法糖
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      '#f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;求值器数据结构

;谓词
(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))

;过程表示
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;环境
;一个环境就是一个框架的序列，每个框架都是一个约束的表格
;其中的约束关联起一些变量和与之对应的值
;(lookup-varaiable-value <var> <env>)
;返回符号<var>在环境<env>里地约束值，如果这一变量没有约束就发出一个错误信号
;(extend-environment <variables> <values> <base-env>)
;返回一个新环境，这个环境中包含了一个新框架，其中的所有位于表<variables>里的符号约束到
;表<valuse>里对应的元素，而其外围环境是环境<base-env>
;(define-variable! <var> <value> <env>)在环境<env>的第一个框架里加入一个新约束
;(set-variable-value! <var> <value> <env>)修改一个约束，如果没有约束则发出错误信号

;我们将环境表示为一个框架的表，环境的外围环境为这个表的cdr，空环境为空表
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;在环境中查找变量:依次在表中寻找所需变量，如果找到了就返回与之对应的值，如果遇到空环境，则发出未约束变量信号
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;在一个环境中为某变量设置新值
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;定义一个变量
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define primitive-procedures  ;基本过程表
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
		(list 'eq? eq?)
		(list 'eqv? eqv?)
		(list 'equal? equal?)
        (list '+ +)
		(list '- -)
		(list '* *)
		(list '/ /)
		(list '= =)
		(list '> >)
		(list '< <)
		(list '>= >=)
		(list '<= <=)
        (list 'display display)
		(list 'exp exp)
		(list 'log log)
		(list 'sin sin)
		(list 'cos cos)
		(list 'asin asin)
		(list 'acos acos)
		(list 'numerator numerator)
		(list 'denominator denominator)
		(list 'floor floor)
		(list 'ceiling ceiling)
		(list 'truncate truncate)
		(list 'round round)
		
        ;<其他基本过程>
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))
;从基本过程表中取出过程体,前面加一个'primitive标识
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))  
       primitive-procedures))
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))



;应用基本过程
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;驱动循环
(define input-prompt "input expression>")
(define output-prompt "output value>")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (error message var)
  (newline) (display message) (display var) (newline))


(define the-global-environment (setup-environment))

;运行求值器:初始化环境,驱动循环
(driver-loop)