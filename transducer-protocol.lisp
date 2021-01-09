(in-package :data-lens.transducers.internals)

(defgeneric reduce-generic (seq func init)
  (:method ((seq sequence) (func function) init)
    (reduce func seq :initial-value init))
  (:method ((seq sequence) (func symbol) init)
    (reduce func seq :initial-value init))
  (:method (seq (func symbol) init)
    (reduce-generic seq
                    (symbol-function func)
                    init))
  (:method ((seq hash-table) (func function) init)
    (let ((acc init))
      (maphash (lambda (k v)
                 (setf acc (funcall func acc (list k v))))
               seq)
      acc)))

#+(or)
(defun document (&rest strings)
  (serapeum:string-join strings #.(format nil "~2%")))

(defgeneric init (client))
(defgeneric stepper (client))
(defgeneric unwrap (client obj)
  (:method (client obj) obj))
(defgeneric builder-for-input (seq)
  (:documentation
   "Take a transducible sequence, return a builder and an init value for that builder.

CONSTRAINT: SEQ should be copied, not modified"))

(defun exit-early (acc)
  (throw 'done acc))

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

(defun into (to xf from)
  (multiple-value-bind (builder init) (builder-for-input to)
    (let* ((xf (etypecase xf
                 (list (apply 'alexandria:compose xf))
                 ((or function symbol) xf)))
           (transducer (funcall xf (stepper builder))))
      (unwrap builder
              (funcall transducer
                       (catch 'done
                         (reduce-generic from
                                         transducer
                                         init)))))))

(defmacro defdocumentation (name &body doc-specs)
  name doc-specs
  nil)

(defdocumentation transducer-protocol
  (:function transduce (xf builder seq)
             "Run a transducer XF over sequence SEQ using BUILDER to accumulate results.

Uses the generic function REDUCE-GENERIC so transducers work over lazy
sequences and hash tables.")
  (:generic-function unwrap (client obj)
                     )
  (:generic-function unwrap (client obj)
                     )
  (:generic-function unwrap (client obj)
                     ))
