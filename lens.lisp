(defpackage :data-lens
  (:use :cl)
  (:export #:regex-match #:include #:exclude #:pick
           #:snapshot-to-vector #:vector-to-lt #:key-transform
           #:combine #:derive #:cumsum #:over #:on #:shortcut
           #:defun-ct))
(in-package :data-lens)

(defmacro shortcut (name function &body bound-args)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf (fdefinition ',name)
           (,function ,@bound-args))))

(defmacro defun-ct (name (&rest args) &body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (defun ,name ,args
       ,@body)))

(defun-ct regex-match (regex)
  (lambda (data)
    (cl-ppcre:scan-to-strings regex data)))

(defun-ct include (pred)
  (lambda (seq)
    (remove-if-not pred seq)))

(defun-ct exclude (pred)
  (lambda (seq)
    (remove-if pred seq)))

(defun-ct pick (selector)
  (lambda (seq)
    (map 'list selector seq)))

(defun-ct key-transform (fun key-get key-set)
  (lambda (it)
    (let ((key-val (funcall key-get it)))
      (funcall key-set
               (funcall fun key-val)))))

(defun-ct combine (fun1 fun2)
  (lambda (item)
    (list (funcall fun1 item)
          (funcall fun2 item))))

(defun-ct derive (diff-fun &key (key #'identity))
  (lambda (list)
    (mapcar (lambda (next cur)
              (cons (funcall diff-fun (funcall key next) (funcall key  cur))
                    next))
            (cdr list)
            list)))

(defun-ct cumsum (&key (add-fun #'+) (key #'identity) (combine (lambda (x y) y x)) (zero 0))
  (lambda (seq)
    (nreverse
     (reduce (lambda (accum next)
               (let ((key-val (funcall key next))
                     (old-val (if accum
                                  (funcall key (car accum))
                                  zero)))
                 (cons (funcall combine
                                (funcall add-fun old-val key-val)
                                next)
                       accum)))
             seq
             :initial-value ()))))

(defun-ct over (fun &key (result-type 'list))
  (lambda (seq)
    (map result-type fun seq)))

(defun-ct on (fun key)
  (lambda (it)
    (funcall fun (funcall key it))))
