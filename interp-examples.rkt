#lang racket
(require "utils.rkt" "fol.rkt")
(define boole
  (interp '(#f #t)
          (lambda (f)
            (case f
              ((zero) (lambda () #f))
              ((one) (lambda () #t))
              ;exclusive or
              ((+) (lambda (a b)
                     (not (eq? a b))))
              ((*) (lambda (a b)
                     (and a b)))
              (else (error 'boole "unknown func symbol ~s" f))))
          (lambda (p)
            (case p
              ((=) (lambda (a b)
                     (eq? a b)))
              (else (error 'boole "unknown pred symbol ~s" p))))))
(define (mod-interp n)
  (interp (range n)
          (lambda (f)
            (case f
              ((zero) (lambda () 0))
              ((one) (lambda () (modulo 1 n)))
              ((+) (lambda (a b)
                     (modulo (+ a b) n)))
              ((*) (lambda (a b)
                     (modulo (* a b) n)))
              (else (error 'mod-interp "unknown func symbol ~s" f))))
          (lambda (p)
            (case p
              ((=) (lambda (a b) (= a b)))
              (else (error 'mod-interp "unknown pred symbol ~s" p))))))
(define modulo_inverse_existence
  '(forall x (=> (not (= x (zero)))
                 (exists y (= (* x y) (one))))))
#;
(filter (lambda (n)
          (holds? (mod-interp n) undefined
                  modulo_inverse_existence))
        (range 1 100))
