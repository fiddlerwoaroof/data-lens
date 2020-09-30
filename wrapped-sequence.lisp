(defpackage :data-lens.wrapped-sequence
  (:use :cl )
  (:export ))
(in-package :data-lens.wrapped-sequence)

(defgeneric underlying (wrapper)
  (:documentation "Return the underlying object of a wrapper"))

(defclass tagged-sequence (standard-object sequence)
  ((%underlying-sequence :initarg :underlying :accessor underlying)
   (%key-fn :initarg :key :reader key)
   (%invert-key :initarg :invert-key :reader invert-key)))

(defmethod sb-sequence:length ((sequence tagged-sequence))
  (length (underlying sequence)))

(defmethod sb-sequence:elt ((sequence tagged-sequence) index)
  (funcall (key sequence)
           (elt (underlying sequence)
                index)))

(defmethod (setf sb-sequence:elt) (new-value (sequence tagged-sequence) index)
  (setf (elt (underlying sequence)
             index)
        (funcall (invert-key sequence)
                 (elt (underlying sequence)
                      index)
                 new-value)))

(defmethod sb-sequence:adjust-sequence ((sequence tagged-sequence) length
                                        &rest r
                                        &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (make-instance 'tagged-sequence
                 :underlying (apply #'sb-sequence:adjust-sequence
                                    (copy-seq (underlying sequence)) length
                                    r)
                 :key-fn (key sequence)
                 :invert-key (invert-key sequence)))

(defmethod sb-sequence:make-sequence-like
    ((sequence tagged-sequence) length &rest r)
  (apply #'sb-sequence:adjust-sequence sequence length r))

(defun wrap-sequence (seq key-fn invert-key-fn)
  (if invert-key-fn
      (make-instance 'tagged-sequence
                     :underlying seq
                     :key key-fn
                     :invert-key invert-key-fn)
      (make-instance 'tagged-sequence
                     :underlying seq
                     :key key-fn)))
