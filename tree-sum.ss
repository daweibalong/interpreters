;; ----- code -----
(define tree-sum
  (lambda (exp)
    (match exp
      [(? number? x) x]
      [`(,e1 ,e2)
       (let ([v1 (tree-sum e1)]
             [v2 (tree-sum e2)])
         (+ v1 v2))])))


;; ----- examples -----
(tree-sum '(1 2))
;; => 3

(tree-sum '(1 (2 3)))
;; => 6

(tree-sum '((1 2) 3))
;; => 6

(tree-sum '((1 2) (3 4)))
;; => 10
