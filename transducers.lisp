(in-package :data-lens.transducers.internals)

(defgeneric unwrap (it obj)
  (:method (it obj) obj))
(defgeneric init (it))
(defgeneric stepper (it))

(defgeneric reduce-generic (seq func init)
  (:method ((seq sequence) (func function) init)
    (reduce func seq :initial-value init))
  (:method ((seq sequence) (func symbol) init)
    (reduce func seq :initial-value init))
  (:method (seq (func symbol) init)
    (reduce-generic seq (symbol-function func) init))
  (:method ((seq hash-table) (func function) init)
    (let ((acc init))
      (maphash (lambda (k v)
                 (setf acc (funcall func acc (list k v))))
               seq)
      acc)))

(in-package :data-lens.transducers)

(declaim (inline mapping filtering deduping catting splitting
                 exit-early taking dropping transduce
                 hash-table-builder vector-builder list-builder))

(defmacro define-functional-transducer (name () &body body)
  `(defun ,name (function &rest args)
     (flet ((call-function (it) (apply function it args)))
       (lambda (rf)
         (lambda (acc next)
           ,@body)))))

(define-functional-transducer mapping ()
  (funcall rf acc (call-function next)))

(define-functional-transducer mv-mapping ()
  (funcall rf acc (multiple-value-list (call-function next))))

(define-functional-transducer mv-selecting ()
  (multiple-value-bind (value use-p) (call-function next)
    (if use-p
        (funcall rf acc value)
        acc)))

(defun hash-table-select (hash-table)
  (mv-selecting #'gethash hash-table))

(define-functional-transducer filtering ()
  (if (call-function next)
      (funcall rf acc next)
      acc))

(defun mv-filtering (function &rest args)
  (filtering (lambda (it)
               (nth-value 1 (apply function it args)))))

(defun deduping (&optional (test 'eql))
  (lambda (rf)
    (let (last)
      (lambda (acc next)
        (prog1 (if (funcall test last next)
                   acc
                   (funcall rf acc next))
          (setf last next))))))

(defun catting ()
  (lambda (rf)
    (lambda (acc next)
      (reduce rf next :initial-value acc))))

(defun splitting (&rest functions)
  (let ((splitter (apply #'data-lens:juxt functions)))
    (mapping splitter)))

(defun exit-early (acc)
  (throw 'done acc))

(defun taking (n)
  (lambda (rf)
    (let ((taken 0))
      (lambda (acc next)
        (incf taken)
        (if (< taken n)
            (funcall rf acc next)
            (exit-early (funcall rf acc next)))))))

(defun dropping (n)
  (lambda (rf)
    (let ((taken 0))
      (lambda (acc next)
        (if (< taken n)
            (progn (incf taken)
                   acc)
            (funcall rf acc next))))))

(defun transduce (xf build seq)
  (unwrap build
          (catch 'done
            (reduce-generic seq
                            (funcall xf (stepper build))
                            (init build)))))
(defun eduction (xf seq)
  (lambda (build)
    (unwrap
     build
     (catch 'done
       (reduce-generic seq
                       (funcall xf (stepper build))
                       (init build))))))

(defmethod init ((it (eql 'hash-table-builder)))
  (make-hash-table))
(defmethod stepper ((it (eql 'hash-table-builder)))
  (lambda (acc next)
    (destructuring-bind (k v) next
      (setf (gethash k acc) v))
    acc))

(defmethod init ((it (eql 'vector-builder)))
  (make-array 0 :fill-pointer t :adjustable t))
(defmethod stepper ((it (eql 'vector-builder)))
  (lambda (acc next)
    (vector-push-extend next acc)
    acc))

(defmethod init ((it (eql 'list-builder)))
  (declare (optimize (speed 3)))
  (let ((it (list nil)))
    (coerce (vector it it)
            '(simple-array list (2)))))
(defmethod stepper ((it (eql 'list-builder)))
  (lambda (acc a)
    (declare (optimize (speed 3))
             (type (simple-array list (2)) acc))
    (let* ((to-build (elt acc 1)))
      (push a (cdr to-build))
      (setf (elt acc 1) (cdr to-build)))
    acc))
(defmethod unwrap ((it (eql 'list-builder)) obj)
  (cdr (elt obj 0)))

(defclass lazy-sequence ()
  ((%next :initarg :next :reader next)))
(defun lazy-sequence (next)
  (make-instance 'lazy-sequence :next next))
(defmethod reduce-generic ((seq lazy-sequence) (func function) init)
  (let ((next (next seq)))
    (loop for next-val = (funcall next)
          for acc = init then next-acc
          for next-acc = (when next-val (funcall func acc next-val))
          while next-val
          finally (return acc))))

(defmacro comment (&body body)
  (declare (ignore body))
  nil)

(comment
  (defun 2* (it)
    (* 2 it))

  (let ((result (transduce (alexandria:compose
                            (catting)
                            (mapping #'parse-integer)
                            (filtering (complement #'evenp))
                            (mapping (data-lens:juxt #'identity
                                                     #'identity))
                            (mapping (data-lens:transform-head #'2*))
                            (mapping (data-lens:transform-head #'1+))
                            (taking 2))
                           'hash-table-builder
                           '(("123" "234" "345" "454")
                             ("568" "490")
                             ("567" "213")))))
    (values result
            (alexandria:hash-table-alist result))))
