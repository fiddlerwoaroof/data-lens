(defpackage :fwoar.lisp-sandbox.t.transducer
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.t.transducer)

(5am:def-suite :data-lens.transducers)
(5am:in-suite :data-lens.transducers)

(5am:def-test mapping (:suite :data-lens.transducers)
  (5am:is (equal '(2 3 4)
                 (data-lens.transducers:transduce (data-lens.transducers:mapping '1+)
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3)))))

(5am:def-test mv-mapping (:suite :data-lens.transducers)
  (5am:is (equal '((2 0) (3 1) (4 2))
                 (data-lens.transducers:transduce (data-lens.transducers:mv-mapping
                                                   (lambda (it)
                                                     (values (1+ it) (1- it))))
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3)))))

(5am:def-test transducer-composition (:suite :data-lens.transducers)
  (5am:is (equal '(3 5 7)
                 (data-lens.transducers:transduce (data-lens:•
                                                   (data-lens.transducers:mapping
                                                    (lambda (x)
                                                      (* 2 x)))
                                                   (data-lens.transducers:mapping '1+))
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3))))
  (5am:is (equal '(3 5 7)
                 (data-lens.transducers:transduce (data-lens:•
                                                   (data-lens.transducers:mapping
                                                    (lambda (x)
                                                      (* 2 x)))
                                                   (data-lens.transducers:mapping '1+))
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3)))))
