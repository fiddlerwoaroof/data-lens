(defpackage :data-lens.t.lens
  (:use :cl )
  (:export ))
(in-package :data-lens.t.lens)

(5am:def-suite :data-lens.lens)
(5am:in-suite :data-lens.lens)

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

(5am:def-test == (:suite :data-lens.lens)
  (5am:is (equal t
                 (funcall (data-lens:== 1)
                          1)))
  (5am:is (equal nil
                 (funcall (data-lens:== (list "1"))
                          (list "1"))))
  (5am:is (equal t
                 (funcall (data-lens:== (list "1") :test #'equal)
                          (list "1")))))

(5am:def-test delay (:suite :data-lens.lens)
  (5am:is (equal '((nil 1)
                   (1 2)
                   (2 3)
                   (3 4))
                 (mapcar (data-lens:juxt (data-lens:delay)
                                         'identity)
                         '(1 2 3 4)))))

(5am:def-test of-length (:suite :data-lens.lens)
  (5am:is (equal t
                 (funcall (data-lens:of-length 3)
                          '(1 2 3))))
  (5am:is (equal nil
                 (funcall (data-lens:of-length 3)
                          '(2 3))))
  (5am:is (equal t
                 (funcall (data-lens:of-length 0)
                          '())))
  (5am:is (equal t
                 (funcall (data-lens:of-length 1)
                          '(1)))))

(5am:def-test of-min-length (:suite :data-lens.lens)
  (5am:is (equal t
                 (funcall (data-lens:of-min-length 3)
                          '(1 2 3 4 5))))
  (5am:is (equal nil
                 (funcall (data-lens:of-min-length 3)
                          '(2 3))))
  (5am:is (equal t
                 (funcall (data-lens:of-min-length 0)
                          '())))
  (5am:is (equal t
                 (funcall (data-lens:of-min-length 0)
                          '(1)))))

(5am:def-test of-max-length (:suite :data-lens.lens)
  (5am:is (equal nil
                 (funcall (data-lens:of-max-length 3)
                          '(1 2 3 4 5))))
  (5am:is (equal t
                 (funcall (data-lens:of-max-length 3)
                          '(2 3))))
  (5am:is (equal t
                 (funcall (data-lens:of-max-length 0)
                          '())))
  (5am:is (equal nil
                 (funcall (data-lens:of-max-length 0)
                          '(1)))))

(5am:def-test applicable-when (:suite :data-lens.lens)
  (5am:is (equal 1
                 (funcall (data-lens:applicable-when '1+ (constantly nil))
                          1)))
  (5am:is (equal "hi"
                 (funcall (data-lens:applicable-when '1+ (constantly nil) "hi")
                          1)))
  (5am:is (equal 2
                 (funcall (data-lens:applicable-when '1+ (constantly t))
                          1))))

(5am:def-test conj (:suite :data-lens.lens)
  (5am:is (equal t
                 (not
                  (not
                   (eval `(funcall (data-lens:conj 'oddp 'identity)
                                   1))))))
  (5am:is (equal nil
                 (not
                  (not
                   (eval `(funcall (data-lens:conj 'oddp 'evenp)
                                   1))))))
  (5am:is (equal t
                 (not
                  (not
                   (eval `(funcall (data-lens:conj)
                                   1)))))))

(5am:def-test disj (:suite :data-lens.lens)
  (5am:is (equal t
                 (not
                  (not
                   (eval `(funcall (data-lens:disj 'oddp 'identity)
                                   1))))))
  (5am:is (equal t
                 (not
                  (not
                   (eval `(funcall (data-lens:disj 'oddp 'evenp)
                                   1))))))
  (5am:is (equal nil
                 (not
                  (not
                   (eval `(funcall (data-lens:disj)
                                   1)))))))

(5am:def-test sorted (:suite :data-lens.lens)
  (5am:is (equal '(1 2 3 4 5)
                 (funcall (data-lens:sorted '<)
                          '(5 4 3 2 1)))))

(5am:def-test element (:suite :data-lens.lens)
  (5am:is (equal 1
                 (funcall (data-lens:element 1)
                          '(0 1 2 3))))
  (5am:is (equal 1
                 (funcall (data-lens:element 1)
                          #(0 1 2 3)))))

(defclass my-map ()
  ((%a :initform 1 :reader a)
   (%b :initform 2 :reader b)
   (%c :initform 3 :reader c)
   (%d :initform 4 :reader d)))
(defmethod data-lens:extract-key ((map my-map) key)
  (string-case:string-case (key)
    ("a" (a map))
    ("b" (b map))
    ("c" (c map))
    ("d" (d map))))

(5am:def-test key (:suite :data-lens.lens)
  (5am:is (equal 1
                 (funcall (data-lens:key "a")
                          (alexandria:alist-hash-table
                           '(("b" . 2)
                             ("a" . 1)
                             ("c" . 3))
                           :test 'equal))))

  (5am:is (equal 1
                 (funcall (data-lens:key "a")
                          '(("b" . 2)
                            ("a" . 1)
                            ("c" . 3)))))
  (5am:is (equal 1
                 (funcall (data-lens:key "a")
                          '("b" 2
                            "a" 1
                            "c" 3))))

  (5am:is (equal 1
                 (funcall (data-lens:key "a")
                          (make-instance 'my-map)))))

(5am:def-test keys (:suite :data-lens.lens)
  (5am:is (equal 4
                 (funcall (data-lens:keys "a" "b" "c" "d")
                          (list (cons "a"
                                      (list "b"
                                            (alexandria:alist-hash-table
                                             (acons "c" (make-instance 'my-map) ())
                                             :test 'equal))))))))

(5am:def-test regex-match (:suite :data-lens.lens)
  (5am:is (serapeum:seq=
           (list "acb" #("c"))
           (multiple-value-list
            (funcall (data-lens:regex-match "a(.)b")
                     "<acb>")))))

(5am:def-test include (:suite :data-lens.lens)
  (5am:is (equal '(1 3 5)
                 (funcall (data-lens:include 'oddp)
                          '(1 2 3 4 5 6)))))

(5am:def-test exclude (:suite :data-lens.lens)
  (5am:is (equal '(2 4 6)
                 (funcall (data-lens:exclude 'oddp)
                          '(1 2 3 4 5 6)))))

(5am:def-test pick (:suite :data-lens.lens)
  (5am:is (equal '(1 2 3)
                 (funcall (data-lens:pick 'car)
                          '((1 2) (2 3) (3 4)))))
  (5am:is (equal '()
                 (funcall (data-lens:pick 'car)
                          '()))))

(5am:def-test slice (:suite :data-lens.lens)
  (5am:is (equal '(1)
                 (funcall (data-lens:slice 1 2)
                          '(0 1 2)))))

(5am:def-test update (:suite :data-lens.lens)
  (5am:is-true (funcall (data-lens:suffixp "qwer")
                        "asdfqwer"))
  (5am:is-true (funcall (data-lens:suffixp (mapcar 'copy-seq
                                                   (list "q" "w" "e" "r"))
                                           :test 'equal)
                        '("a" "s" "d" "f" "q" "w" "e" "r")))
  (5am:is-false (funcall (data-lens:suffixp "qwer")
                         "qwerasdf"))
  (5am:is-false (funcall (data-lens:suffixp (mapcar 'copy-seq
                                                    (list "q" "w" "e" "r"))
                                            :test 'equal)
                         '("q" "w" "e" "r" "a" "s" "d" "f"))))

(5am:def-test suffixp (:suite :data-lens.lens))

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
  (5am:is (equalp #(2 3 4)
                  (funcall (data-lens:over '1+ :result-type 'vector '1+)
                           '(0 1 2))))
  (5am:is (equalp #(2 3 4)
                  (funcall (data-lens:over '1+ '1+ :result-type 'vector)
                           '(0 1 2))))
  (5am:is (equalp #(1 2 3)
                  (funcall (data-lens:over '1+ :result-type 'vector)
                           #(0 1 2)))))

(5am:def-test transform-elt (:suite :data-lens.lens :depends-on (and functionalize))
  (5am:is (equal '(1 1 2)
                 (funcall (data-lens:transform-elt 0 '1+)
                          '(0 1 2))))
  (5am:is (equal '(0 2 2)
                 (funcall (data-lens:transform-elt 1 '1+)
                          '(0 1 2))))
  (5am:is (equalp #(0 1 3)
                  (funcall (data-lens:transform-elt 2 '1+)
                           (vector 0 1 2))))
  (5am:is (equal "Abc"
                 (funcall (data-lens:transform-elt 0 'char-upcase)
                          "abc"))))

(5am:def-test transform (:suite :data-lens.lens :depends-on (and functionalize))
  (5am:is (equal (funcall (data-lens:transform 1) #'1+)
                 2))

  (5am:is (equal (funcall (data-lens:transform 1)
                          (data-lens:juxt '1- 'identity '1+))
                 '(0 1 2))))

(5am:def-test calling (:suite :data-lens.lens :depends-on (and functionalize))
  (5am:is (equal (funcall (data-lens:calling #'- 1) 3)
                 2))
  (5am:is (equal (funcall (data-lens:calling #'- 2 1) 3)
                 0)))

(5am:def-test calling* (:suite :data-lens.lens :depends-on (and functionalize))
  (5am:is (equal (funcall (data-lens:calling* #'- 3) 1)
                 2))
  (5am:is (equal (funcall (data-lens:calling* #'- 3 2) 1)
                 0)))
