#lang racket


;; ----- code -----
(struct Scope (table parent))
(struct Closure (f env))

(define new-env
  (lambda (env)
    (Scope (make-hash) env)))

(define assign
  (lambda (x v env)
    (hash-set! (Scope-table env) x v)))

(define env0 (new-env #f))

(define ext-env
  (lambda (x v env)
    (let ([env+ (new-env env)])
      (assign x v env+)
      env+)))

(define lookup
  (lambda (x env)
    (cond
     [(not env) #f]
     [else
      (let ([v? (hash-ref (Scope-table env) x #f)])
        (cond
         [(not v?)
          (lookup x (Scope-parent env))]
         [else v?]))])))

(define interp
  (lambda (exp env)
    (match exp
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
          [(not v)
           (error "undefined variable" x)]
          [else v]))]
      [(? number? x) x]
      [`(lambda (,x) ,e)
       (Closure exp env)]
      [`(let ([,x ,e1]) ,e2 ...)
       (let ([v1 (interp e1 env)])
         (interp `(begin ,@e2) (ext-env x v1 env)))]
      [`(define ,x ,e)
       (let ([v1 (interp e env)])
         (assign x v1 env))]
      [`(begin ,e1 ... ,en)
       (for ([e e1])
         (interp e env))
       (interp en env)]
      [`(if ,t ,e1 ,e2)
       (let ([tval (interp t env)])
         (if tval
             (interp e1 env)
             (interp e2 env)))]
      [`(,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (interp e (ext-env x v2 env-save))]))]
      [`(,op ,e1 ,e2)
       (let ([v1 (interp e1 env)]
             [v2 (interp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]
           ['= (= v1 v2)]
           ['< (< v1 v2)]
           ['> (> v1 v2)]))])))


(define r3
  (lambda (exp)
    (interp exp env0)))


;; ----- examples -----
(r3
 '(begin
    (define x 1)
    (define y 2)
    (+ x y)))
;; => 3


(r3
 '(begin
    (let ([x 1])
      (define f (lambda (y) (+ x y)))
      (let ([x 2])
        (f 0)))))
;; => 1


(r3
 '(begin
    (define x 1)
    (define f (lambda (y) (+ x y)))
    (let ([x 2])
      (f 0))))
;; => 1


(r3
 '(begin
    (define x 1)
    (define f (lambda (y) (+ x y)))
    (define x 2)
    (f 0)))
;; => 2


(r3
 '(begin
    (define fact
      (lambda (n)
        (if (= n 0)
            1
            (* n (fact (- n 1))))))
    (fact 5)))
;; => 120


(r3
 '(begin
    (define fib
      (lambda (n)
        (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2))))))
    (fib 9)))
;; => 34
