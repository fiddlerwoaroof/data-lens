(in-package :data-lens.transducers)

(declaim (inline mapping filtering deduping catting splitting
                 exit-early taking dropping transduce
                 hash-table-builder vector-builder list-builder))

(defmacro transducer-lambda (&body (((two-arg-acc two-arg-next) &body two-arg-body)
                                    &optional (((one-arg-arg) &body one-arg-body)
                                               '((it) it))))
  (alexandria:with-gensyms (arg1 arg2 next-sym-p)
    `(lambda (,arg1 &optional (,arg2 nil ,next-sym-p))
       (if ,next-sym-p
           (let ((,two-arg-acc ,arg1)
                 (,two-arg-next ,arg2))
             ,@two-arg-body)
           (let ((,one-arg-arg ,arg1))
             ,@one-arg-body)))))


(defun mapping (function &rest args)
  (flet ((call-function (it)
           (apply function it args)))
    (lambda (rf)
      (transducer-lambda
        ((acc next)
         (funcall rf acc (call-function next)))
        ((it) (funcall rf it))))))

(defun mv-mapping (function &rest args)
  (flet ((call-function (it)
           (apply function it args)))
    (lambda (rf)
      (transducer-lambda
        ((acc next)
         (funcall rf acc
                  (multiple-value-list (call-function next))))
        ((it) (funcall rf it))))))

(defun mv-selecting (function &rest args)
  (flet ((call-function (it)
           (apply function it args)))
    (lambda (rf)
      (transducer-lambda
        ((acc next)
         (multiple-value-bind (value use-p)
             (call-function next)
           (if use-p
               (funcall rf acc value)
               acc)))
        ((it) (funcall rf it))))))

(defun hash-table-select (hash-table)
  (mv-selecting #'gethash hash-table))

(defun filtering (function &rest args)
  (flet ((call-function (it)
           (apply function it args)))
    (lambda (rf)
      (transducer-lambda
        ((acc next)
         (if (call-function next)
             (funcall rf acc next)
             acc))
        ((it) (funcall rf it))))))

(defun mv-filtering (function &rest args)
  (filtering (lambda (it)
               (nth-value 1 (apply function it args)))))

(defun seq (a b) a b)
(defun compressing-runs (&key (test 'eql) (combiner 'seq))
  (lambda (rf)
    (let (last leftovers)
      (transducer-lambda
        ((acc next)
         (if (or (null last)
                 (funcall test last next))
             (progn (setf last (funcall combiner last next)
                          leftovers t)
                    acc)
             (progn (prog1 (funcall rf acc last)
                      (setf last (funcall combiner last next))))))
        ((it)
         (funcall rf
                  (if leftovers
                      (funcall rf it last)
                      it)))))))


(defun deduping (&optional (test 'eql))
  (compressing-runs :test test))

(defun catting ()
  (lambda (rf)
    (transducer-lambda
      ((acc next)
       (reduce-generic next rf acc))
      ((it) (funcall rf it)))))

(defun mapcatting (fun)
  (data-lens:• (mapping fun)
               (catting)))

(defun splitting (&rest functions)
  (let ((splitter (apply #'data-lens:juxt functions)))
    (mapping splitter)))

(defun exit-early (acc)
  (throw 'done acc))

(defun taking (n)
  (lambda (rf)
    (let ((taken 0))
      (transducer-lambda
        ((acc next)
         (incf taken)
         (if (<= taken n)
             (funcall rf acc next)
             (exit-early acc)))
        ((it) (funcall rf it))))))

(defun dropping (n)
  (lambda (rf)
    (let ((taken 0))
      (transducer-lambda
        ((acc next)
         (if (< taken n)
             (progn (incf taken)
                    acc)
             (funcall rf acc next)))
        ((it) (funcall rf it))))))

(defun transduce (xf build seq)
  (let* ((xf (etypecase xf
               (list (apply 'alexandria:compose xf))
               ((or function symbol) xf)))
         (transducer (funcall xf (stepper build))))
    (unwrap build
            (funcall transducer
                     (catch 'done
                       (reduce-generic seq
                                       transducer
                                       (init build)))))))
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
  (transducer-lambda
    ((acc next)
     (destructuring-bind (k v) next
       (setf (gethash k acc) v))
     acc)))

(defmethod init ((it (eql 'vector-builder)))
  (make-array 0 :fill-pointer t :adjustable t))
(defmethod stepper ((it (eql 'vector-builder)))
  (transducer-lambda
    ((acc next)
     (vector-push-extend next acc)
     acc)))

(defmethod init ((it (eql 'list-builder)))
  (declare (optimize (speed 3)))
  (let ((it (list nil)))
    (coerce (vector it it)
            '(simple-array list (2)))))
(defmethod stepper ((it (eql 'list-builder)))
  (transducer-lambda
    ((acc a)
     (declare (optimize (speed 3))
              (type (simple-array list (2)) acc))
     (let* ((to-build (elt acc 1)))
       (push a (cdr to-build))
       (setf (elt acc 1) (cdr to-build)))
     acc)))
(defmethod unwrap ((it (eql 'list-builder)) obj)
  (cdr (elt obj 0)))

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
