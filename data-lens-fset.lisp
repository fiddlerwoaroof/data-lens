(defpackage :fwoar.data-lens-fset
  (:use :cl )
  (:export
   #:make-seq-lens
   #:make-bag-lens
   #:make-set-lens))
(in-package :fwoar.data-lens-fset)

(named-readtables:in-readtable fset:fset-readtable)

(defmethod data-lens.transducers.internals:reduce-generic ((set fset:set) (func function) init)
  (fset:reduce func set :initial-value init))
(defmethod data-lens.transducers.internals:builder-for-input ((seq fset:set))
  (values 'fset-set-builder
          seq))
(defmethod data-lens.transducers.internals:stepper ((seq (eql 'fset-set-builder)))
  (data-lens.transducers:transducer-lambda
    ((acc next)
     (fset:with acc next))))
(defmethod data-lens:functionalize ((it fset:set))
  (lambda (key)
    (nth-value 1 (fset:lookup it key))))
(defmethod data-lens:extract-key ((it fset:set) key)
  (nth-value 1 (fset:lookup it key)))
(defun make-set-lens (item)
  "A lens for updating a set"
  (lambda (cb)
    (lambda (set)
      (data-lens.lenses:fmap (lambda (new)
                               (fset:with (fset:less set item) new))
                             (funcall cb (nth-value 1 (fset:lookup set item)))))))
(defmethod data-lens.lenses:generic-lens ((rec fset:set) cb loc)
  (funcall (funcall (make-set-lens loc)
                    cb)
           rec))

(defmethod data-lens.transducers.internals:reduce-generic ((seq fset:seq) (func function) init)
  (fset:reduce func seq :initial-value init))
(defmethod data-lens.transducers.internals:builder-for-input ((seq fset:seq))
  (values 'fset-seq-builder
          seq))
(defmethod data-lens.transducers.internals:stepper ((seq (eql 'fset-seq-builder)))
  (data-lens.transducers:transducer-lambda
    ((acc next)
     (fset:with-last acc next))))
(defmethod data-lens:functionalize ((it fset:seq))
  (lambda (key)
    (fset:lookup it key)))
(defmethod data-lens:extract-key ((it fset:seq) key)
  (fset:lookup it key))
(defmethod data-lens.lenses:fmap (function (data fset:seq)))
(defun make-seq-lens (index)
  "A lens for updating a sequence"
  (check-type index (integer 0))
  (lambda (cb)
    (lambda (seq)
      (data-lens.lenses:fmap (lambda (new)
                               (fset:with seq index new))
                             (funcall cb (fset:lookup seq index))))))
(defmethod data-lens.lenses:generic-lens ((rec fset:seq) cb (loc integer))
  (funcall (funcall (make-seq-lens loc)
                    cb)
           rec))


(defmethod data-lens.transducers.internals:reduce-generic ((map fset:map) (func function) init)
  (fset:reduce (lambda (acc k v)
                 (funcall func acc (list k v)))
               map
               :initial-value init))
(defmethod data-lens.transducers.internals:builder-for-input ((map fset:map))
  (values 'fset-map-builder
          map))
(defmethod data-lens.transducers.internals:stepper ((map (eql 'fset-map-builder)))
  (data-lens.transducers:transducer-lambda
    ((acc next)
     (destructuring-bind (k v) next
       (fset:with acc k v)))))
(defmethod data-lens:functionalize ((it fset:map))
  (lambda (key)
    (fset:lookup it key)))
(defmethod data-lens:extract-key ((it fset:map) key)
  (fset:lookup it key))
(defun make-map-lens (key)
  (lambda (cb)
    (lambda (map)
      (data-lens.lenses:fmap (lambda (new)
                               (fset:with map key new))
                             (funcall cb (fset:lookup map key))))))
(defmethod data-lens.lenses:generic-lens ((rec fset:map) cb loc)
  (funcall (funcall (make-map-lens loc)
                    cb)
           rec))


(defmethod data-lens.transducers.internals:builder-for-input ((bag fset:bag))
  (values 'fset-bag-builder
          bag))
(defmethod data-lens.transducers.internals:stepper ((bag (eql 'fset-bag-builder)))
  (data-lens.transducers:transducer-lambda
    ((acc next)
     (fset:with acc next))))
(defmethod data-lens:functionalize ((it fset:bag))
  (lambda (key)
    (fset:multiplicity it key)))
(defmethod data-lens:extract-key ((it fset:bag) key)
  (let ((m (fset:multiplicity it key)))
    (values key
            m)))
(defun make-bag-lens (item)
  (make-set-lens item))

(defmethod data-lens.lenses:generic-lens ((rec fset:bag) cb loc)
  (funcall (funcall (make-set-lens loc)
                    cb)
           rec))
