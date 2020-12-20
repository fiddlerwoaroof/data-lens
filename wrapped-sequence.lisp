(defpackage :data-lens.wrapped-sequence
  (:use :cl )
  (:export ))
(in-package :data-lens.wrapped-sequence)

(defgeneric underlying (wrapper)
  (:documentation "Return the underlying object of a wrapper"))

(defgeneric key (tagged-sequence))
(defgeneric invert-key (tagged-sequence))
(defclass tagged-sequence (standard-object
                           org.shirakumo.trivial-extensible-sequences:sequence)
  ((%underlying-sequence :initarg :underlying :accessor underlying)))



(defmethod org.shirakumo.trivial-extensible-sequences:length
    ((sequence tagged-sequence))
  (length (underlying sequence)))

(defmethod org.shirakumo.trivial-extensible-sequences:elt
    ((sequence tagged-sequence) index)
  (funcall (key sequence)
           (elt (underlying sequence)
                index)))

(defmethod (setf org.shirakumo.trivial-extensible-sequences:elt)
    (new-value (sequence tagged-sequence) index)
  (setf (elt (underlying sequence)
             index)
        (funcall (invert-key sequence)
                 (elt (underlying sequence)
                      index)
                 new-value)))

(defmethod org.shirakumo.trivial-extensible-sequences:adjust-sequence
    ((sequence tagged-sequence) length
     &rest r
     &key initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (unless (slot-boundp sequence '%underlying-sequence)
    (setf (underlying sequence) ()))
  (fw.lu:prog1-bind (it (make-instance (class-of sequence)
                                       :underlying (apply
                                                    #'org.shirakumo.trivial-extensible-sequences:adjust-sequence
                                                    (copy-seq (underlying sequence)) length
                                                    r)))
    (describe it)))

(defmethod org.shirakumo.trivial-extensible-sequences:make-sequence-like
    ((sequence tagged-sequence) length &rest r)
  (apply #'org.shirakumo.trivial-extensible-sequences:adjust-sequence
         sequence length r))

(defun wrap-sequence (class seq)
  (make-instance class
                 :underlying seq))
