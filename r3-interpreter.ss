#lang racket

(struct Scope (table parent))
(struct Closure (f env))

(define env0 (Scope (make-hash) #f))

(define ext-env
  (lambda (env)
    (let* ([new-hash (make-hash)]
           [new-env (Scope new-hash env)])
      new-env)))

(define def
  (lambda (x v env)
    (hash-set! (Scope-table env) x v)))

(define bind
  (lambda (x v env)
    (let (new-env (ext-env env))
      (def x v new-env)
      new-env)))

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
      [`(let ([,x ,e1]) ,e2)
       (let ([v1 (interp e1 env)])
         (interp e2 (ext-env x v1 env)))]
      [`(define ,x ,e)
       (let ([v1 (interp e env)])
         (def x v1 env))]
      [`(begin ,e1 ... ,en)
       (for ([e e1])
         (interp e env))
       (interp en env)]
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
           ['/ (/ v1 v2)]))])))


(define interpreter
  (lambda (exp)
    (interp exp env0)))
