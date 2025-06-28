(in-package :data-lens.lenses)

#+fw.dev
(progn
  ;; maybe functor implementation
  (defclass maybe ()
    ())
  (defclass just (maybe)
    ((%v :initarg :value :reader value)))
  (defclass nothing (maybe)
    ())

  (defun just (value)
    (make-instance 'just :value value))
  (defun nothing (&optional value)
    (declare (ignore value))
    (make-instance 'nothing))

  (defgeneric maybe (default value)
    (:method (default (value just))
      (value value))
    (:method (default (value nothing))
      default))

  (defgeneric maybe-apply (function value)
    (:method (function (value just))
      (just (funcall function (value value))))
    (:method (function (value nothing))
      value))

  (defmethod print-object ((o just) s)
    (format s "#.(~s ~s)"
            'just
            (value o)))

  (defmethod print-object ((o nothing) s)
    (format s "#.(~s)"
            'nothing)))

;; identity functor, necessary for set and over
(defclass identity- ()
  ((%v :initarg :value :reader unidentity)))

(defun wrap-identity (v)
  (make-instance 'identity- :value v))

(defmethod print-object ((o identity-) s)
  (format s "#.(~s ~s)"
          'wrap-identity
          (unidentity o)))

;; constant functor, necessary for view
(defclass constant- ()
  ((%v :initarg :value :reader unconstant)))

(defun wrap-constant (v)
  (make-instance 'constant- :value v))

(defmethod print-object ((o constant-) s)
  (format s "#.(~s ~s)"
          'wrap-constant
          (unconstant o)))

(defgeneric fmap (function data)
  (:method (function (data identity-))
    (wrap-identity
     (funcall function
              (unidentity data))))
  (:method (function (data constant-))
    data)
  (:method (function (data list))
    (mapcar function data))
  (:method (function (data vector))
    (map 'vector function data))
  #+fw.dev
  (:method (function (data maybe))
    (maybe-apply function data)))

(defun over (lens cb rec)
  "Given a lens, a callback and a record, apply the lens to the
record, transform it by the callback and return copy of the record,
updated to contain the result of the callback. This is the fundamental
operation on a lens and SET and VIEW are implemented in terms of it.

A lens is any function of the form (lambda (fun) (lambda (rec) ...))
that obeys the lens laws (where == is some reasonable equality
operator):

    (== (view lens (set lens value rec))
        value)

    (== (set lens (view lens rec) rec)
        rec)

    (== (set lens value2 (set lens value1 rec))
        (set lens value2 rec))

The inner lambda returns a functor that determines the policy to be
applied to the focused part.  By default, this only uses IDENTITY- and
CONSTANT- in order to implement the lens operations over, set and
view.

If these conditions are met, (over (data-lens:<>1 lens1 lens2) ...) is
equivalent to using lens2 to focus the part lens1 focuses: note that
composition is \"backwards\" from what one might expect: this is
because composition composes the wrapper lambdas and applies the
lambda that actually pulls a value out of a record later."
  (unidentity
   (funcall (funcall lens (lambda (x) (wrap-identity (funcall cb x))))
            rec)))

(defun view (lens rec)
  "Given a lens and a rec, return the focused value"
  (unconstant
   (funcall (funcall lens (lambda (x) (wrap-constant x)))
            rec)))

(defun set (lens v rec)
  "Given a lens, a value and a rec, immutably update the rec to
contain the new value at the location focused by the lens."
  (unidentity
   (funcall (funcall lens (lambda (_) _ (wrap-identity v)))
            rec)))

#+fw.dev
(progn
  ;; "fake" functors that don't assume a functor result to their
  ;; callback
  (defun over* (lens cb rec)
    (funcall (funcall lens cb)
             rec))

  (defun set* (lens value rec)
    (over lens
          (lambda (_)
            (declare (ignore _))
            value)
          rec))

  (defun view* (lens rec)
    (over lens
          (lambda (value)
            (return-from view*
              value))
          rec)))

(defun make-alist-history-lens (key)
  "A lens for updating a alist, preserving previous values"
  (lambda (cb)
    (lambda (alist)
      (fmap (lambda (new)
              (cons (cons key new)
                    alist))
            (funcall cb (cdr (assoc key alist)))))))

(defun make-alist-lens (key)
  "A lens for updating a alist, discarding previous values"
  (lambda (cb)
    (lambda (alist)
      (fmap (lambda (new)
              (remove-duplicates (cons (cons key new)
                                       alist)
                                 :key #'car
                                 :from-end t))
            (funcall cb (cdr (assoc key alist)))))))

(defun make-list-lens (index)
  "A lens for updating a sequence"
  (lambda (cb)
    (lambda (seq)
      (fmap (lambda (new)
              (let ((result (copy-seq seq)))
                (prog1 result
                  (setf (elt result index) new))))
            (funcall cb (elt seq index))))))

(defun make-plist-lens (key)
  "A lens for updating a plist, preserving previous values"
  (lambda (cb)
    (lambda (plist)
      (fmap (lambda (new)
              (list* key new
                     plist))
            (funcall cb (getf plist key))))))

(defun make-hash-table-lens (key)
  "A lens for updating a hash-table, discarding previous values"
  (lambda (cb)
    (lambda (old-hash)
      (fmap (lambda (new)
              (let ((new-hash (alexandria:copy-hash-table old-hash)))
                (prog1 new-hash
                  (setf (gethash key new-hash)
                        new))))
            (funcall cb (gethash key old-hash))))))

;; imagine a lens here that uses the MOP to immutably update a class...
(defgeneric clone (obj &rest new-initargs &key)
  (:method :around (obj &rest new-initargs &key)
    (apply #'reinitialize-instance (call-next-method) new-initargs)))

#+fw.demo
(progn
  (defclass foo ()
    ((a :initarg :a :accessor a)))
  (defmethod clone ((obj foo) &key)
    (make-instance 'foo :a (a obj)))

  ;;; needs to be updated for functor-based lens
  (defun a-lens (cb)
    (lambda (foo)
      (fw.lu:prog1-bind (new (clone foo))
        (setf (a new)
              (funcall cb (a foo))))))
  (view 'a-lens
        (over 'a-lens '1+
              (set 'a-lens 2
                   (make-instance 'foo :a 1)))) #|
  ==> 3 |#)

(defgeneric generic-lens (rec cb loc)
  (:method ((rec hash-table) cb loc)
    (funcall (funcall (make-hash-table-lens loc)
                      cb)
             rec))
 (:method ((rec vector) cb loc)
    (funcall (funcall (make-list-lens loc)
                      cb)
             rec)))

(defun lens (loc)
  "extensible lens using a multimethod for internal implementation"
  (lambda (cb)
    (lambda (rec)
      (generic-lens rec cb loc))))
