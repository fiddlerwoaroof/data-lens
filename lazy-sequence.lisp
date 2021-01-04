(in-package :data-lens.transducers)

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

(defmacro repeating (v)
  `(lazy-sequence
    (lambda ()
      ,v)))

(defun repeating* (v &key count)
  (lazy-sequence
   (if count
       (let ((iterations 0))
         (lambda ()
           (when (< iterations count)
             (prog1 v
               (incf iterations)))))
       (lambda ()
         v))))

(defun iota (&key (start 0) (step 1) count)
  (lazy-sequence
   (funcall
    (compile nil
             `(lambda ()
                (declare (optimize (speed 3) (debug 1) (safety 1)))
                (let ((init ,start)
                      ,@(serapeum:unsplice (when count
                                             '(iterations 0))))
                  (declare (type (integer ,start
                                          ,(if count
                                               (+ start (* count step))
                                               '*))
                                 init)
                           ,@(serapeum:unsplice
                              (when count
                                `(type (integer 0 ,count) iterations))))
                  (lambda ()
                    (declare (optimize (speed 3)))
                    (,@(if count
                           `(when (< iterations ,count))
                           '(progn))
                     (prog1 init
                       (incf init ,step)
                       ,@(serapeum:unsplice
                          (when count
                            '(incf iterations))))))))))))
