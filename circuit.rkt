#lang racket
(require "utils.rkt" "prop.rkt")
(define X_ (make-index 'X))
(define Y_ (make-index 'Y))
(define OUT_ (make-index 'OUT))
(define C_ (make-index 'C))
(define C0_ (make-index 'C0))
(define C1_ (make-index 'C1))
(define C2_ (make-index 'C2))
(define S_ (make-index 'S))
(define S0_ (make-index 'S0))
(define S1_ (make-index 'S1))
(define S2_ (make-index 'S2))
(define (halfsum x y)
  `(<=> ,x (not ,y)))
(define (halfcarry x y)
  `(and ,x ,y))
(define (ha x y s c)
  `(and (<=> ,s ,(halfsum x y))
        (<=> ,c ,(halfcarry x y))))
(define (carry x y z)
  `(or (and ,x ,y)
       (and (or ,x ,y) ,z)))
(define (sum x y z)
  (halfsum (halfsum x y) z))
(define (fa x y z s c)
  `(and (<=> ,s ,(sum x y z))
        (<=> ,c ,(carry x y z))))
(define (ripplecarry x y c out n)
  (conjoin
   (lambda (i)
     (fa (x i) (y i) (c i) (out i) (c (+ i 1))))
   (range n)))
(define (ripplecarry0 x y c out n)
  (psimplify
   (ripplecarry
    x y (lambda (i) (if (= i 0) #f (c i)))
    out n)))
(define (ripplecarry1 x y c out n)
  (psimplify
   (ripplecarry
    x y (lambda (i) (if (= i 0) #t (c i)))
    out n)))
(define (mux sel in0 in1)
  `(or (and (not ,sel) ,in0)
       (and ,sel ,in1)))
(define ((offset k x) i)
  (x (+ i k)))
(define (carryselect x y c0 c1 s0 s1 c s n k)
  (define k^ (min n k))
  (define exp0
    `(and (and ,(ripplecarry0 x y c0 s0 k^)
               ,(ripplecarry1 x y c1 s1 k^))
          (and (<=> ,(c k^) ,(mux (c 0) (c0 k^) (c1 k^)))
               ,(conjoin
                 (lambda (i)
                   `(<=> ,(s i) ,(mux (c 0) (s0 i) (s1 i))))
                 (range k^)))))
  (if (= (- n k^) 0)
      exp0
      `(and ,exp0
            ,(carryselect
              (offset k x) (offset k y) (offset k c0) (offset k c1)
              (offset k s0) (offset k s1) (offset k c) (offset k s)
              (- n k) k))))
(define (carryselect^ x y c0 c1 s0 s1 c s n k)
  (define k^ (min n k))
  (define exp0
    `(and (and ,(ripplecarry0 x y c0 s0 k^)
               ,(ripplecarry1 x y c1 s1 k^))
          (and (<=> ,(c k^) ,(mux (c 0) (c0 k^) (c1 k^)))
               ,(conjoin
                 (lambda (i)
                   `(<=> ,(s i) ,(mux (c 0) (s0 i) (s1 i))))
                 (range k^)))))
  (if (< k^ k)
      exp0
      `(and ,exp0
            ,(carryselect^
              (offset k x) (offset k y) (offset k c0) (offset k c1)
              (offset k s0) (offset k s1) (offset k c) (offset k s)
              (- n k) k))))
(define (make-adder-test n k)
  `(=> (and (and ,(carryselect X_ Y_ C0_ C1_ S0_ S1_ C_ S_ n k)
                 (not ,(C_ 0)))
            (and ,(ripplecarry X_ Y_ C2_ S2_ n)
                 (not ,(C2_ 0))))
       (and (<=> ,(C_ n) ,(C2_ n))
            ,(conjoin
              (lambda (i)
                `(<=> ,(S_ i) ,(S2_ i)))
              (range n)))))
;(dplltaut? (make-adder-test 5 3))
;> (psimplify (carryselect X_ Y_ C0_ C1_ S0_ S1_ C_ S_ 0 1))
;'(<=> C_0 (or (and (not C_0) C0_0) (and C_0 C1_0)))
;; > (define test0
;;     `(=> (and (<=> c_2k ,(mux 'c_k 'c0_2k 'c1_2k))
;;               (=> c0_2k c1_2k))
;;          (<=> c_2k ,(mux 'c_2k 'c0_2k 'c1_2k))))
;; > (tautology? test0)
;; #t
;; > (tautology?
;;    `(=> (and ,(ripplecarry0 X_ Y_ C_ S_ 3)
;;              ,(ripplecarry1 X_ Y_ C1_ S1_ 3))
;;         (=> ,(C_ 3) ,(C1_ 3))))
;; #t
;; > (dplltaut?
;;    `(=> ,(make-conj
;;           (list (carryselect X_ Y_ C0_ C1_ S0_ S1_ C_ S_ 4 2)
;;                 '(<=> C_0 #f)
;;                 '(<=> X_0 #f)
;;                 '(<=> X_1 #t)
;;                 '(<=> X_2 #f)
;;                 '(<=> X_3 #t)
;;                 '(<=> Y_0 #t)
;;                 '(<=> Y_1 #t)
;;                 '(<=> Y_2 #t)
;;                 '(<=> Y_3 #t)))
;;         ,(make-conj
;;           (list '(<=> S_0 #t)
;;                 '(<=> S_1 #f)
;;                 '(<=> S_2 #f)
;;                 '(<=> S_3 #t)
;;                 '(<=> C_4 #t)))))
;; #t
