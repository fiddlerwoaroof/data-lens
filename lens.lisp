(defpackage :data-lens
  (:use :cl)
  (:export #:regex-match #:include #:exclude #:pick
           #:snapshot-to-vector #:vector-to-lt #:key-transform
           #:combine #:derive #:cumsum #:over #:on #:shortcut
           #:defun-ct
           #:key
           #:extract-key
           #:element
           #:let-fn
           #:juxt))
(in-package :data-lens)

(defmacro shortcut (name function &body bound-args)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf (fdefinition ',name)
           (,function ,@bound-args))))

(defmacro defun-ct (name (&rest args) &body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (defun ,name ,args
       ,@body)))

(defmacro let-fn ((&rest bindings) &body body)
  (let ((binding-forms (mapcar (lambda (form)
                                 `(,(car form) ,(cadr form)
                                    (funcall ,@(cddr form) ,@(cadr form))))
                               bindings)))
    `(labels ,binding-forms
       ,@body)))

(defgeneric extract-key (map key)
  (:method ((map hash-table) key)
    (gethash key map))
  (:method ((map list) key)
    (typecase (car map)
      (cons (cdr (assoc key map :test 'equal)))
      (t (loop for (a-key . value) on map by #'cddr
            when (equal key a-key) do
              (return (car value)))))))

(defun-ct deduplicate (&optional (test 'eql))
  (lambda (it)
    (remove-duplicates it :test test)))

(defun cons-new (&key (test 'eql) (key 'identity))
  (lambda (acc next)
    (if (and acc
             (funcall test
                      (funcall key (car acc))
                      (funcall key next)))
        acc
        (cons next acc))))

(defun matching-list-reducer (test acc next)
  (if (and acc
           (funcall test (caar acc) (car next)))
      (cons (cons (caar acc)
                  (append (cdar acc)
                          (cdr next)))
            (cdr acc))
      (cons next acc)))

(defun combine-matching-lists (&key (test 'eql) &allow-other-keys)
  (lambda (acc next)
    (matching-list-reducer test acc next)))

(defun-ct compress-runs (&key (collector 'cons-new) (test 'eql) (key 'identity))
  (lambda (it)
    (nreverse
     (reduce (funcall collector :test test :key key)
             it
             :initial-value ()))))

(defun-ct sorted (comparator &rest r &key key)
  (declare (ignore key))
  (lambda (it)
    (apply #'stable-sort (copy-seq it) comparator r)))

(defun-ct element (num)
  (lambda (it)
    (elt it num)))

(defun-ct key (key)
  (lambda (map)
    (declare (dynamic-extent map))
    (extract-key map key)))

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

(defun-ct juxt (fun1 fun2)
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
