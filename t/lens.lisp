(defpackage :data-lens.t.lens
  (:use :cl )
  (:export ))
(in-package :data-lens.t.lens)

(5am:def-suite :data-lens.lens)
(5am:in-suite :data-lens.lens)

(5am:def-test == (:suite :data-lens.lens)
  (5am:is (equal t
                 (funcall (data-lens:== 1)
                          1)))
  (5am:is (equal nil
                 (funcall (data-lens:== (list "1"))
                          (list "1"))))
  (5am:is (equal nil
                 (funcall (data-lens:== (list "1") :test #'equal)
                          (list "1")))))

(5am:def-test functionalize (:suite :data-lens.lens)
  (5am:is (equal 2
                 (funcall (data-lens:functionalize #'1+) 1)))
  (5am:is (equal 0
                 (funcall (data-lens:functionalize '1-) 1)))
  (5am:is (equal 3
                 (funcall (data-lens:functionalize #(0 3)) 1)))
  (5am:is (equal 8
                 (funcall (data-lens:functionalize
                           (alexandria:plist-hash-table '(1 8 2 4)))
                          1))))

(5am:def-test on (:suite :data-lens.lens :depends-on (and functionalize))
  (5am:is (equal 2
                 (funcall (data-lens:on '1+ 'car)
                          '(1 2))))
  (5am:is (equal 5
                 (funcall (data-lens:on '+ 'car)
                          '(1 2)
                          '(4 5))))
  (5am:is (equal 13
                 (funcall (data-lens:on '+ 'car)
                          '(1 2)
                          '(4 5)
                          '(8 9)))))

(5am:def-test over (:suite :data-lens.lens :depends-on (and functionalize))
  (5am:is (equal '(1 2 3)
                 (funcall (data-lens:over '1+)
                          '(0 1 2))))
  (5am:is (equal '(1 2 3)
                 (funcall (data-lens:over '1+)
                          #(0 1 2))))
  (5am:is (equalp #(1 2 3)
                  (funcall (data-lens:over '1+ :result-type 'vector)
                           '(0 1 2))))
  (5am:is (equalp #(1 2 3)
                  (funcall (data-lens:over '1+ :result-type 'vector)
                           #(0 1 2)))))
