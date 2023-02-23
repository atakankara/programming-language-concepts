#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(require trace)

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))

      (list-exp ()
        (list-val '())
      )

      (cons-exp (exp1 lst)
                (let ((val1 (num-val exp1))
                      (lstval (value-of lst env)))
                (list-val (cons exp1 (expval->lst lstval))))
                )

      (sum-exp (lst)
               (let ((lstval (value-of lst env)))
                 (num-val (sum (expval->lst lstval)))))

      (rational-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (rational-val (create-rational num1 num2))
                    )))
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                              ((= op 3) (/ num1 num2))
                              (#t (- num1 num2))
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ((= op 3) (cons (* num1 num2bot) num2top))
                              (#t (cons (- (* num1 num2bot) num2top) num2bot))
                              
                              ))))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ((= op 3) (cons num1top (* num1bot num2)))
                              (#t (cons (- num1top (* num2 num1bot)) num1bot))
                              ))))

                        (#t
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ((= op 3) (cons (* num1top num2bot) (* num2top num1bot))) ;; divide
                              (#t (cons (- (* num1top num2bot) (* num2top num1bot)) (* num1bot num2bot)))
                            ))))))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                        (if (zero? num1)
                          (bool-val #t)
                          (bool-val #f))
                          ; -----------------------
                          ; INSERT YOUR CODE HERE 
                          ; -----------------------
      
                        (if (= (cdr num1) 0)
                            (eopl:error 'zero-exp "Denominator of rational is 0")
                            (if (= (car num1) 0)
                                (bool-val #t)
                                (bool-val #f))
                            )
                        ))))

      

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------

      ;; -----------------------
      (simpl-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                         (num-val num1)
                         (let ((top (car num1))
                               (bottom (cdr num1)))
                           (rational-val (cons (/ top (gcd top bottom)) (/ bottom (gcd top bottom))))
                         )
                   )
                 )))

      )))
(define gcd
  (lambda (top bottom)
    (if (= (modulo top bottom) 0)
        bottom
        (gcd bottom (modulo top bottom))
  )))

(define sum
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum (cdr lst)))       
    )))

(define create-rational
  (lambda (num1 num2)
    (if (= num2 0)
        (eopl:error 'create-rationals "Denominator is 0")
        (cons num1 num2)
  )))