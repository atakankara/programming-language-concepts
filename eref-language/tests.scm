(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)


      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      (nested-procs2 "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
        -1)
      
       (y-combinator-1 "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)
      
       ;; simple letrecs
      (simple-letrec-1 "letrec f(x) = -(x,1) in (f 33)" 32)
      (simple-letrec-2
        "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
        8)

      (simple-letrec-3
        "let m = -5 
 in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
        20)
      
;      (fact-of-6  "letrec
;  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
;in (fact 6)" 
;                  720)
      
      (HO-nested-letrecs
"letrec even(odd)  = proc(x) if zero?(x) then 1 else (odd -(x,1))
   in letrec  odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1))
   in (odd 13)" 1)

      
      (begin-test-1
        "begin 1; 2; 3 end"
        3)

      (gensym-test-1 
"let g = let counter = newref(0) 
         in proc (dummy) let d = setref(counter, -(deref(counter),-1))
                    in deref(counter)
in -((g 11),(g 22))"
       -1)

      (simple-store-test-1 "let x = newref(17) in deref(x)" 17)

      (assignment-test-1 "let x = newref(17) 
                          in begin setref(x,27); deref(x) end"
        27)

      (gensym-test-2 
"let g = let counter = newref(0) 
         in proc (dummy) begin
                           setref(counter, -(deref(counter),-1));
                           deref(counter)
                         end
 in -((g 11),(g 22))"
       -1)

     (even-odd-via-set-1 "
let x = newref(0)
in letrec even(d) = if zero?(deref(x)) 
                   then 1
                   else let d = setref(x, -(deref(x),1))
                        in (odd d)
          odd(d)  = if zero?(deref(x)) 
                   then 0
                   else let d = setref(x, -(deref(x),1))
                        in (even d)
   in let d = setref(x,13) in (odd -100)" 1)

 (even-odd-via-set-1 "
let x = newref(0)
in letrec even(d) = if zero?(deref(x)) 
                   then 1
                   else let d = setref(x, -(deref(x),1))
                        in (odd d)
          odd(d)  = if zero?(deref(x)) 
                   then 0
                   else let d = setref(x, -(deref(x),1))
                        in (even d)
   in let d = setref(x,13) in (odd -100)" 1)

 (show-allocation-1 "
let x = newref(22)
in let f = proc (z) let zz = newref(-(z,deref(x))) in deref(zz)
   in -((f 66), (f 55))"
   11)

 (chains-1 "
let x = newref(newref(0))
in begin 
    setref(deref(x), 11);
    deref(deref(x))
   end"
   11)

      ; ==================== Vector test cases =========================

   (vector-detailed-test-1 "let a = newvector(2, -99) in
                              let p = proc (x)
                                  let v = read-vector(x, 1)
                                  in update-vector(x, 1, -(v, -1))
                       in begin update-vector(a, 1, 0); (p a); (p a); read-vector(a, 1) end"
                      2)

      (vector-detailed-test-2 "let a = newvector(3, 5) in
                              let p = proc (x)
                                   let v = read-vector(x, 1)
                                   in update-vector(x, 1, -(-2, v))
                              in let q = proc(x)
                                  let v1 = read-vector(x, 1) in 
                                  let v2 = read-vector(x, 2)    
                                  in update-vector(x, 1, -(v2, -(0, v1)))
                       in begin update-vector(a, 1, -5); (p a); (q a); read-vector(a, 1) end"
                      8)

      (vector-detailed-test-3 "let a = newvector(2, -99) in
                              let p = proc (x)
                                  let v = read-vector(x, 1)
                                  in update-vector(v, 1, -(read-vector(v, 2), -(-1, read-vector(v, 1))))
                       in begin update-vector(a, 1, newvector(3,4)); (p a); (p a); (p a); read-vector(read-vector(a, 1), 1) end"
                      19)
;
;
;      (vector-detailed-test-4 "let a = newvector(5, -99) in
;                               let v = copy-vector(a)
;                                  in begin
;                                     update-vector(a, 0, 1); update-vector(a, 1, 2); update-vector(a, 2, 3); update-vector(a, 3, 4);
;                                     -(-(0, read-vector(v, 0)), read-vector(v, 1))         
;                                  end"
;                      198)
;
      (vector-detailed-test-5 "let a = newvector(5, 1) in
                                  begin
                                    update-vector(a, 0, 0); update-vector(a, 1, 1); update-vector(a, 2, 2); update-vector(a, 3, 3);
                                     swap-vector(a, 0, 3);
                                     -(read-vector(a, 0), read-vector(a,1))
                                  end"
                      2)

      (vector-detailed-test-6 "let a = newvector(101, 45) in
                                let b = newvector(56, 34) in
                                    -(length-vector(b), -(0, length-vector(a)))"
                      157)

      (vector-detailed-test-7 "let a = newvector(89, 78) in
                                let b = copy-vector(a) in
                                    length-vector(b)"
                      89)

      (vector-detailed-test-8 "let a = newvector(89, 78) in
                                let b = copy-vector(a) in
                                    begin
                                    update-vector(b, 0, 5); swap-vector(b, 0, 1); -(length-vector(a), -(0, read-vector(b, 1)))
                                    end"
                      94)

 
            ; ==================== Stack test cases =========================;

      (stack-test1 "let x = newstack(5) in begin push(x, 10); push(x, 20); push(x,30); stack-size(x) end" 3)
      (stack-test2 "let x = newstack(6) in begin push(x, 10); push(x, 20); pop(x); pop(x); push(x, 30); peek(x) end" 30)
      (stack-test3 "let x = newstack(6) in begin push(x, 10); push(x, 20); push(x,30); pop(x); pop(x); pop(x); empty-stack?(x) end" #t)
      (stack-test4 "let x = newstack(6) in begin push(x, 10); pop(x); push(x, 20); push(x, 30); pop(x); peek(x) end" 20)
      (stack-test5 "let x = newstack(6) in begin push(x, 10); pop(x); push(x, 20); push(x, 30); pop(x); peek(x); push(x, 30); stack-size(x) end" 2)
      (stack-test6 "let x = newstack(6) in let y= newstack(6) in begin push(x, 10); pop(x); push(x, 20); push(x, 30); pop(x); peek(x); push(y, 50); push(y, 18); push(y, 26); push(x, 10); push(x, 50); stack-size(y) end" 3)
      (stack-test6 "let x = newstack(6) in let y= newstack(6) in begin push(x, 10); pop(x); push(x, 20); push(x, 30); pop(x); peek(x); push(y, 50); push(y, 18); push(y, 26); push(x, 10);  pop(y); push(x, 50); peek(y) end" 18)
  
;;;;;;;; This test case should pass and it should print 50 20. ;
      (stack-test7 "let x = newstack(5) in begin push(x, 10); pop(x); push(x, 20); push(x, 30); pop(x); peek(x); push(x, 50); print-stack(x); peek(x) end" 50) 


           ; ==================== Vector Multiplication Test Cases =========================;

      (vec-mult-test1 "let x = newvector(3, 0) in let y = newvector(3, 0) in begin update-vector(x, 0, 1); update-vector(x, 1, 2); update-vector(x, 2, 3); update-vector(y, 0, 4); update-vector(y, 1, 5); update-vector(y, 2, 6); read-vector(vec-mult(x, y),0) end" 4)
      (vec-mult-test2 "let x = newvector(3, 0) in let y = newvector(3, 0) in begin update-vector(x, 0, 1); update-vector(x, 1, 2); update-vector(x, 2, 3); update-vector(y, 0, 4); update-vector(y, 1, 5); update-vector(y, 2, 6); read-vector(vec-mult(x, y),1) end" 10)
      (vec-mult-test3 "let x = newvector(3, 0) in let y = newvector(3, 0) in begin update-vector(x, 0, 1); update-vector(x, 1, 2); update-vector(x, 2, 3); update-vector(y, 0, 4); update-vector(y, 1, 5); update-vector(y, 2, 6); read-vector(vec-mult(x, y),2) end" 18)

 



 ))
  )
