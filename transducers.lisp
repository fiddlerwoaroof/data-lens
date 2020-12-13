(in-package :data-lens.transducers.beta)

(defun mapping (function)
  (lambda (rf)
    (lambda (acc next)
      (funcall rf acc (funcall function next)))))

(defun filtering (predicate)
  (lambda (rf)
    (lambda (acc next)
      (if (funcall predicate next)
          (funcall rf acc next)
          acc))))

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
  (funcall build
           (catch 'done
             (reduce (funcall xf build) seq :initial-value (funcall build)))))

(defmacro comment (&body body)
  (declare (ignore body))
  nil)

(defun hash-table-builder (&optional (acc nil acc-p) (next nil next-p))
  (cond (next-p (destructuring-bind (k v) next
                  (setf (gethash k acc) v)) acc)
        (acc-p acc)
        (t (make-hash-table))))

(defun vector-builder (&optional (acc nil acc-p) (next nil next-p))
  (cond (next-p (vector-push-extend next acc) acc)
        (acc-p acc)
        (t (make-array 0 :fill-pointer t :adjustable t))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (labels ((make-snoc ()
             (vector nil nil))
           (add-to-snoc (acc a)
             (if (elt acc 1)
                 (let* ((to-build (elt acc 1)))
                   (push a (cdr to-build))
                   (setf (elt acc 1) (cdr to-build)))
                 (let ((new (list a)))
                   (setf (elt acc 0) new
                         (elt acc 1) new)))
             acc)
           (desnoc (acc)
             (elt acc 0)))
    (defun list-builder (&optional (acc nil acc-p) (next nil next-p))
      (cond (next-p (add-to-snoc acc next))
            (acc-p (desnoc acc))
            (t (make-snoc))))))

(comment
  (defun 2* (it)
    (* 2 it))

  (let ((result (transduce (alexandria:compose (catting)
                                               (mapping #'parse-integer)
                                               (filtering (complement #'evenp))
                                               (mapping (data-lens:juxt #'identity #'identity))
                                               (mapping (data-lens:transform-head #'2*))
                                               (mapping (data-lens:transform-head #'1+))
                                               (taking 2))
                           'hash-table-builder
                           '(("123" "234" "345" "454") ("568" "490") ("567" "213")))
                ))
    (values result
            (alexandria:hash-table-alist result))))
