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
