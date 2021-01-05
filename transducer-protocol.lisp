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

(defgeneric init (client))
(defgeneric stepper (client))
(defgeneric unwrap (client obj)
  (:method (client obj) obj))

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

#+(or)
(defdocumentation transducer-protocol
    (:function transduce (xf build seq)
               )
  (:generic-function unwrap (client obj)
                     )
  (:generic-function unwrap (client obj)
                     )
  (:generic-function unwrap (client obj)
                     ))
