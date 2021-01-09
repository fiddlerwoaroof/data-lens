(defpackage :data-lens.t.transducers
  (:use :cl )
  (:export ))
(in-package :data-lens.t.transducers)

(5am:def-suite :data-lens.transducers)
(5am:in-suite :data-lens.transducers)

(5am:def-test mapping (:suite :data-lens.transducers)
  (5am:is (equal '(2 3 4)
                 (data-lens.transducers:transduce (data-lens.transducers:mapping '1+)
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3)))))

(5am:def-test mv-mapping (:suite :data-lens.transducers
                          :depends-on mapping)
  (5am:is (equal '((2 0) (3 1) (4 2))
                 (data-lens.transducers:transduce (data-lens.transducers:mv-mapping
                                                   (lambda (it)
                                                     (values (1+ it) (1- it))))
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3)))))

(5am:def-test mv-selecting (:suite :data-lens.transducers
                            :depends-on mapping)
  (5am:is (equal '(2 4)
                 (data-lens.transducers:transduce (data-lens.transducers:mv-selecting
                                                   (lambda (it)
                                                     (values (1+ it) (oddp it))))
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3))))
  (5am:is (equal '(a b)
                 (data-lens.transducers:transduce (data-lens.transducers:hash-table-select
                                                   (alexandria:alist-hash-table
                                                    '((1 . a)
                                                      (2 . b))))
                                                  'data-lens.transducers:list-builder
                                                  '(1 2)))))

(5am:def-test filtering (:suite :data-lens.transducers
                         :depends-on mapping)
  (5am:is (equal '(1 3)
                 (data-lens.transducers:transduce (data-lens.transducers:filtering 'oddp)
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3))))
  (5am:is (equal '(1 2)
                 (data-lens.transducers:transduce (data-lens.transducers:mv-filtering
                                                   'gethash (alexandria:alist-hash-table
                                                             '((1 . a)
                                                               (2 . b))))
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3)))))

(5am:def-test deduping (:suite :data-lens.transducers
                        :depends-on mapping)
  (5am:is (equal '(1 2 1 2 3 4 1)
                 (data-lens.transducers:transduce (data-lens.transducers:deduping)
                                                  'data-lens.transducers:list-builder
                                                  '(1 1 2 1 2 2 3 3 4 1))))
  (loop for first-number = (random 4)
        for second-number = (+ 5 (random 4))
        for third-number = (+ 10 (random 4))
        repeat 10 do
          (5am:is
           (equal (data-lens.transducers:transduce
                   (data-lens:• (data-lens.transducers:catting)
                                (data-lens.transducers:deduping))
                   'data-lens.transducers:list-builder
                   (list (data-lens.transducers:repeating* first-number :count (+ 1 (random 10)))
                         (data-lens.transducers:repeating* second-number :count (+ 1 (random 5)))
                         (data-lens.transducers:repeating* third-number :count (+ 1 (random 3)))))
                  (list first-number second-number third-number)))))

(5am:def-test compressing-runs (:suite :data-lens.transducers
                                :depends-on mapping)
  (5am:is (equal '(1 2 1 2 3 4 1)
                 (data-lens.transducers:transduce (data-lens.transducers:compressing-runs)
                                                  'data-lens.transducers:list-builder
                                                  '(1 1 2 1 2 2 3 3 4 1))))
  (loop for input = '(1) then (cons 1 input)
        repeat 5 do
          (5am:is (equal '(1)
                         (data-lens.transducers:transduce (data-lens.transducers:compressing-runs)
                                                          'data-lens.transducers:list-builder
                                                          input))))
  (loop for input = '(2 1) then (cons 2 input)
        repeat 5 do
          (5am:is (equal '(2 1)
                         (data-lens.transducers:transduce (data-lens.transducers:compressing-runs)
                                                          'data-lens.transducers:list-builder
                                                          input))))
  (loop for input = '(1 2) then (append input '(2))
        repeat 5 do
          (5am:is (equal '(1 2)
                         (data-lens.transducers:transduce (data-lens.transducers:compressing-runs)
                                                          'data-lens.transducers:list-builder
                                                          input))))

  (5am:is
   (equal '((1 2 2 2 3 3 3)
            (2 4 4 4 5 5 5)
            (3 6 6 6))
          (data-lens.transducers:transduce
           (data-lens.transducers:compressing-runs
            :test (lambda (a b) (eql (car a) (car b)))
            :combiner (lambda (a b)
                        (if a
                            (append a (cdr b))
                            b)))
           'data-lens.transducers:list-builder
           '((1 2 2 2) (1 3 3 3)
             (2 4 4 4) (2 5 5 5)
             (3 6 6 6))))))

(5am:def-test catting (:suite :data-lens.transducers
                       :depends-on mapping)
  (5am:is (equal '(1 1 2 1 2 2 3 3 4 1)
                 (data-lens.transducers:transduce (data-lens.transducers:catting)
                                                  'data-lens.transducers:list-builder
                                                  '((1 1) (2 1 2) (2) (3 3) (4 1))))))

(5am:def-test mapcatting (:suite :data-lens.transducers
                          :depends-on catting)
  (5am:is (equal '(1 1 2 1 2 2 3 3 4 1)
                 (data-lens.transducers:transduce (data-lens.transducers:mapcatting 'list)
                                                  'data-lens.transducers:list-builder
                                                  '(1 1 2 1 2 2 3 3 4 1))))
  (5am:is (equal '(0 0 1 0 1 2)
                 (data-lens.transducers:transduce (data-lens.transducers:mapcatting 'identity)
                                                  'data-lens.transducers:list-builder
                                                  (list (data-lens.transducers:iota :count 1)
                                                        (data-lens.transducers:iota :count 2)
                                                        (data-lens.transducers:iota :count 3))))))

(5am:def-test collecting (:suite :data-lens.transducers
                          :depends-on mapcatting)
  (5am:is (equal '(1 3 6 9 13 18 22 27 33 40)
                 (data-lens.transducers:transduce (data-lens:• (data-lens.transducers:taking 5)
                                                               (data-lens.transducers:mapcatting 'identity)
                                                               (data-lens.transducers::collecting '+))
                                                  'data-lens.transducers:list-builder
                                                  (loop for x from 1
                                                        repeat 4
                                                        collect
                                                        (data-lens.transducers:iota :start x
                                                                                    :count x))))))

(5am:def-test splitting (:suite :data-lens.transducers
                         :depends-on mapping)
  (5am:is (equal '((2 0) (3 1) (4 2))
                 (data-lens.transducers:transduce (data-lens.transducers:splitting '1+ '1-)
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3)))))

(5am:def-test transducer-composition (:suite :data-lens.transducers
                                      :depends-on (and filtering mapping))
  (5am:is (equal '(7)
                 (data-lens.transducers:transduce (data-lens:•
                                                   (data-lens.transducers:filtering #'oddp)
                                                   (data-lens.transducers:mapping
                                                    (lambda (x)
                                                      (* 2 x)))
                                                   (data-lens.transducers:mapping '1+)
                                                   (data-lens.transducers:dropping 1))
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3))))
  (5am:is (equal '(3 5)
                 (data-lens.transducers:transduce (data-lens:•
                                                   (data-lens.transducers:taking 2)
                                                   (data-lens.transducers:mapping
                                                    (lambda (x)
                                                      (* 2 x)))
                                                   (data-lens.transducers:mapping '1+))
                                                  'data-lens.transducers:list-builder
                                                  '(1 2 3)))))

(5am:def-test complicated (:suite :data-lens.transducers
                           :depends-on (and catting mapping filtering splitting
                                            transducer-composition))
  (let ((result (data-lens.transducers:transduce
                 (data-lens:•
                  (data-lens.transducers:catting)
                  (data-lens.transducers:mapping #'parse-integer)
                  (data-lens.transducers:filtering (complement #'evenp))
                  (data-lens.transducers:splitting (serapeum:op (* 2 _)) #'identity)
                  (data-lens.transducers:mapping (data-lens:transform-head #'1+))
                  (data-lens.transducers:taking 3))
                 'data-lens.transducers:hash-table-builder
                 '(("123" "234" "345" "454")
                   ("568" "490")
                   ("567" "213")))))
    (5am:is (equal '((247 . 123)
                     (691 . 345)
                     (1135 . 567))
                   (funcall (data-lens:• (data-lens:sorted '< :key 'car)
                                         (data-lens:sorted '< :key 'cdr))
                            (alexandria:hash-table-alist result))))))
