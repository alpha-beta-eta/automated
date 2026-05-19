#lang racket
(require "utils.rkt" "fol.rkt")
(define p20
  '(=> (forall
        x (forall
           y (exists
              z (forall w (=> (and (P x) (Q y))
                              (and (R z) (U w)))))))
       (=> (exists x (exists y (and (P x) (Q y))))
           (exists z (R z)))))
(define p21
  '(=> (and (exists x (=> (p) (F x)))
            (exists x (=> (F x) (p))))
       (exists x (<=> (p) (F x)))))
(define p29
  '(=> (and (exists x (F x))
            (exists x (G x)))
       (<=> (and (forall x (=> (F x) (H x)))
                 (forall x (=> (G x) (J x))))
            (forall x (forall y (=> (and (F x) (G y))
                                    (and (H x) (J y))))))))
(define p36
  '(=> (and (forall x (exists y (F x y)))
            (and (forall x (exists y (G x y)))
                 (forall
                  x (forall
                     y (=> (or (F x y) (G x y))
                           (forall z (=> (or (F y z) (G y z))
                                         (H x z))))))))
       (forall x (exists y (H x y)))))
(define p39
  '(not (exists x (forall y (<=> (F y x) (not (F y y)))))))
