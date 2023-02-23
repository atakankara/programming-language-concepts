(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require racket/trace)
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################

(newvector-exp (expression1 expression2)
            (let ((length (expval->num (value-of expression1 env)))
                  (vector (newref (value-of expression2 env))))
                  (begin (new-reference (- length 1) (value-of expression2 env)) (vec-val (vector-elements length vector)))))


(update-vector-exp (expression1 expression2 expression3)
                  (let ((vector (get-vector-values (value-of expression1 env))) (index (expval->num (value-of expression2 env)))
                        (value (value-of expression3 env))) (setref! (+ vector index) value)))

(read-vector-exp (expression1 expression2)
            (let ((vector (get-vector-values (value-of expression1 env)))
                  (index (expval->num (value-of expression2 env)))) (deref (+ vector index))))
        
(length-vector-exp (expression1) (num-val (get-vector-length (value-of expression1 env))))

(swap-vector-exp (expression1 expression2 expression3)
            (let ((vector (get-vector-values (value-of expression1 env)))
                  (index1 (expval->num (value-of expression2 env))) (index2 (expval->num (value-of expression3 env))))
                  (let ((value1 (deref (+ vector index1))) (value2 (deref (+ vector index2))))
                  (begin (setref! (+ vector index1) value2) (setref! (+ vector index2) value1)))))

(copy-vector-exp (expression1) (whole-vector-copy (value-of expression1 env)))

(newstack-exp (expression1) (stack-val (newref 0) (create-new-vec (expval->num(value-of expression1 env)))))

(push-exp (expression1 expression2) (let ((stack (value-of expression1 env)) (value (value-of expression2 env)))
      (let ((top (get-stack-top stack)) (vector (get-stack-values stack)))
      (begin (setref! (+ vector (deref top)) value) (setref! top (+ (deref top) 1))))))

        (pop-exp (expression) (let ((stack (value-of expression env)))
      (let ((top (get-stack-top stack)) (vector (get-stack-values stack)))
      (let ((max_index (deref top))) (if (eqv? 0 max_index) (num-val -1) (begin (setref! top (- max_index 1)) (deref (+ vector max_index))))))))

(stack-size-exp (expression) (let ((top (get-stack-top (value-of expression env)))) (num-val (deref top))))

(empty-stack?-exp (exp) (let ((top (get-stack-top (value-of exp env)))) (bool-val (eqv? (deref top) 0))))

(peek-exp (expression) (let ((stack (value-of expression env)))
      (let ((top (get-stack-top stack)) (vector (get-stack-values stack)))
      (let ((max-index (deref top))) (if (eqv? 0 max-index) (num-val -1) (deref (- (+ vector max-index) 1)))))))

(print-stack-exp (expression) (let ((stack (value-of expression env))) (let ((top (deref (get-stack-top stack))) 
      (vector (get-stack-values stack))) (print-stack top vector))))

 (vec-mult-exp (expression1 expression2) (let ((vector-exp1 (value-of expression1 env)) (vector-exp2 (value-of expression2 env)))
      (let ((vector-values-1 (get-vector-values vector-exp1)) (length1 (get-vector-length vector-exp1)) 
            (vector-values-2 (get-vector-values vector-exp2)) (length2 (get-vector-length vector-exp2)))
            (if (not (eq? length1 length2))
            'length-of-vectors-should-be-the-same
            (let ((output-vector (vec-val (create-new-vec length1))))
                  (begin (multiply-vectors-helper vector-values-1 vector-values-2 (get-vector-values output-vector) length1)
                  output-vector
                  )
            )))))
 )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE

 (define single-copy
    (lambda (exp)
      (cases expval exp (ref-val (ref) (ref-val (newref (single-copy (deref ref)))))
        (vec-val (vec) (whole-vector-copy exp)) (else exp))))

  (define print-stack
    (lambda (top vector)
      (if (eqv? top 0) (display "")
          (begin (display (expval->num (deref (+ vector (- top 1)))))
            (display " ") (print-stack (- top 1) vector)))))

  (define create-new-vec
    (lambda (length)
      (let ((vector (newref (num-val 0))))
        (begin (new-reference (- length 1) 0) (vector-elements length vector)))))

  (define new-reference
    (lambda (length values)
      (if (zero? length) 1
          (begin (newref values) (new-reference (- length 1) values)))))
        
  (define whole-vector-copy
    (lambda (vector)
      (let ((values (get-vector-values vector)) (length  (get-vector-length vector)))
        (let ((copy-vector (newref (num-val 0)))) (begin
            (new-reference (- length 1) 0) (multiple-copy values copy-vector (- length 1) )
            (vec-val (vector-elements length copy-vector)))))))

 (define (multiply-vectors-helper vector1 vector2 output length)
    (if (> length 0)
        (let ((value1 (expval->num (deref vector1))) (value2 (expval->num (deref vector2))))
      (begin (setref! output (num-val (* value1 value2))) (multiply-vectors-helper (+ vector1 1) (+ vector2 1) (+ output 1) (- length 1)))
      )'placeholder))

  (define multiple-copy
    (lambda (vector copy-vector num_of_elem)
      (if (eqv? num_of_elem -1) 1
          (begin
            (setref! (+ copy-vector num_of_elem) (single-copy (deref (+ vector num_of_elem))))
            (multiple-copy vector copy-vector  (- num_of_elem 1))))))

   
    (define get-at-index
    (lambda (vec index)
      (deref (+ (get-vector-values vec) index))))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
