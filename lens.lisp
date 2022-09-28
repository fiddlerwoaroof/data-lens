(in-package :data-lens)

(declaim
 (inline data-lens:over data-lens:transform-tail
         data-lens:applicable-when data-lens:of-min-length
         data-lens:on data-lens:over data-lens:slice
         data-lens:compress-runs data-lens:combine-matching-lists
         data-lens:juxt data-lens:element data-lens:sorted))

(defgeneric functionalize (it)
  (:method ((it hash-table))
    (lambda (key &optional default)
      (gethash key it default)))
  (:method ((it vector))
    (lambda (idx &optional default)
      (let ((present-p (and (>= idx 0)
                            (< idx (length it)))))
        (values (if present-p
                    (aref it idx)
                    default)
                present-p))))
  (:method ((it symbol))
    (fdefinition it))
  (:method ((it function))
    it))

;;; TODO: consider making this wrap defalias?
(defmacro shortcut (name function &body bound-args)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf (fdefinition ',name)
           (,function ,@bound-args))))

(defmacro defun-ct (name (&rest args) &body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (defun ,name ,args
       ,@body)))

(defmacro let-fn ((&rest bindings) &body body)
  (let ((binding-forms (mapcar (lambda (form)
                                 `(,(car form) ,(cadr form)
                                   (funcall ,@(cddr form) ,@(cadr form))))
                               bindings)))
    `(labels ,binding-forms
       ,@body)))

(defgeneric extract-key (map key)
  (:method ((map hash-table) key)
    (gethash key map))
  (:method ((map list) key)
    (typecase (car map)
      (cons (cdr (assoc key map :test 'equal)))
      (t (loop for (a-key . value) on map by #'cddr
               when (equal key a-key) do
                 (return (car value)))))))

(defun-ct == (target &key (test 'eql))
  (lambda (v)
    (funcall test target v)))

(defun-ct deduplicate (&optional (test 'eql))
  (lambda (it)
    (remove-duplicates it :test test)))

(defun cons-new (&key (test 'eql) (key 'identity))
  (lambda (acc next)
    (if (and acc
             (funcall test
                      (funcall key (car acc))
                      (funcall key next)))
        acc
        (cons next acc))))

(defun matching-list-reducer (test acc next)
  (if (and acc
           (funcall test (caar acc) (car next)))
      (cons (cons (caar acc)
                  (append (cdar acc)
                          (cdr next)))
            (cdr acc))
      (cons next acc)))

(defun combine-matching-lists (&key (test 'eql) &allow-other-keys)
  (lambda (acc next)
    (matching-list-reducer test acc next)))

(defun-ct compress-runs (&key (collector 'cons-new) (test 'eql) (key 'identity))
  (lambda (it)
    (nreverse
     (reduce (funcall collector :test test :key key)
             it
             :initial-value ()))))

(defun-ct of-length (len)
  (lambda (it)
    (= (length it)
       len)))

(defun-ct of-min-length (len)
  (lambda (it)
    (>= (length it)
        len)))

(defun-ct of-max-length (len)
  (lambda (it)
    (>= (length it)
        len)))

(defun-ct applicable-when (fun test)
  (lambda (it)
    (if (funcall test it)
        (funcall fun it)
        it)))

(defmacro conj (&rest fns)
  (let ((dat (gensym "dat")))
    `(lambda (,dat)
       (and ,@(mapcar (lambda (fn)
                        `(funcall ,fn ,dat))
                      fns)))))

(defmacro disj (&rest fns)
  (let ((dat (gensym "dat")))
    `(lambda (,dat)
       (or ,@(mapcar (lambda (fn)
                       `(funcall ,fn ,dat))
                     fns)))))


(defun-ct sorted (comparator &rest r &key key)
  (declare (ignore key))
  (lambda (it)
    (apply #'stable-sort (copy-seq it) comparator r)))

(defun-ct element (num)
  (lambda (it)
    (elt it num)))

(defun-ct key (key)
  (lambda (map)
    (declare (dynamic-extent map))
    (extract-key map key)))

(defun keys (key &rest keys)
  (lambda (map)
    (loop for key in (cons key keys)
          for cur = (extract-key map key) then (extract-key cur key)
          finally (return cur))))

(defun-ct regex-match (regex)
  (lambda (data)
    (cl-ppcre:scan-to-strings regex data)))

(defun-ct include (pred)
  (lambda (seq)
    (remove-if-not pred seq)))

(defun-ct exclude (pred)
  (lambda (seq)
    (remove-if pred seq)))

(defun-ct pick (selector)
  (lambda (seq)
    (map 'list selector seq)))

(defun slice (start &optional end)
  (lambda (it)
    (subseq it start end)))

(defun-ct update (thing fun &rest args)
  (apply fun thing args))

(define-modify-macro updatef (fun &rest args)
  update)

(defun-ct suffixp (suffix &key (test 'eql test-p))
  (lambda (it)
    (if test-p
        (alexandria:ends-with-subseq suffix
                                     it
                                     :test test)
        (alexandria:ends-with-subseq suffix
                                     it))))

(defun-ct transform-head (fun)
  (lambda (it)
    (typecase it
      (list (list* (funcall fun (car it))
                   (cdr it)))
      (vector (let ((result (copy-seq it)))
                (prog1 result
                  (updatef (elt result 0) fun)))))))

(defun-ct transform-tail (fun)
  (lambda (it)
    (typecase it
      (list (list* (car it)
                   (funcall fun (cdr it))))
      (vector (let ((result (copy-seq it)))
                (prog1 result
                  (updatef (subseq result 1)
                           fun)))))))

(defun-ct splice-elt (elt fun)
  (lambda (it)
    (append (subseq it 0 elt)
            (funcall fun (nth elt it))
            (subseq it (1+ elt)))))

(defun-ct transform-elt (elt fun)
  (lambda (it)
    (append (subseq it 0 elt)
            (list (funcall fun (nth elt it)))
            (subseq it (1+ elt)))))

(defun-ct key-transform (fun key-get key-set)
  (lambda (it)
    (let ((key-val (funcall key-get it)))
      (funcall key-set
               (funcall fun key-val)))))

(defun-ct juxt (fun1 &rest r)
  (lambda (&rest args)
    (list* (apply fun1 args)
           (when r
             (mapcar (lambda (f)
                       (apply f args))
                     r)))))

(defun =>> (fun1 fun2)
  (lambda (i)
    (prog1 (funcall fun1 i)
      (funcall fun2))))

(defun-ct derive (diff-fun &key (key #'identity))
  (lambda (seq)
    (typecase seq
      (list (cons (cons nil (car seq))
                  (mapcar (lambda (next cur)
                            (cons (funcall diff-fun
                                           (funcall key next)
                                           (funcall key  cur))
                                  next))
                          (cdr seq)
                          seq)))
      (vector (coerce (loop for cur = nil then next
                            for next across seq
                            if cur
                              collect (cons (funcall diff-fun
                                                     (funcall key next)
                                                     (funcall key cur))
                                            cur)
                            else collect (cons nil next))
                      'vector)))))

(defun-ct inc (inc)
  (declare (optimize (speed 3)))
  (lambda (base)
    (+ base inc)))

(defun-ct cumsum
    (&key (add-fun #'+) (key #'identity) (combine (lambda (x y) y x)) (zero 0))
  (lambda (seq)
    (nreverse
     (reduce (lambda (accum next)
               (let ((key-val (funcall key next))
                     (old-val (if accum
                                  (funcall key (car accum))
                                  zero)))
                 (cons (funcall combine
                                (funcall add-fun old-val key-val)
                                next)
                       accum)))
             seq
             :initial-value ()))))

(defun-ct over (fun &key (result-type 'list))
  (let ((fun (functionalize fun)))
    (lambda (seq)
      (map result-type fun seq))))

(defun-ct denest (&key (result-type 'list))
  (lambda (seq)
    (apply #'concatenate result-type
           seq)))

(defmacro applying (fun &rest args)
  (alexandria:with-gensyms (seq fsym)
    `(let ((,fsym (functionalize ,fun)))
       (lambda (,seq)
         (apply ,fsym ,@args ,seq)))))

(defun-ct on (fun key)
  "Transform arguments with KEY and then apply FUN

> (eql (funcall (on 'equal 'car)
>          '(\"a\" 1 2)
>          '(\"a\" 2 e))
>      t)"
  (let ((fun (functionalize fun))
        (key (functionalize key)))
    (lambda (&rest its)
      (apply fun (mapcar key its)))))

(defun filler (length1 length2 fill-value)
  (if (< length1 length2)
      (make-sequence 'vector (- length2 length1) :initial-element fill-value)
      #()))

(defun-ct zipping (result-type &key (fill-value nil fill-value-p))
  (lambda (seq1 seq2)
    (let ((length1 (when fill-value-p (length seq1)))
          (length2 (when fill-value-p (length seq2))))
      (let ((seq1 (if fill-value-p
                      (concatenate result-type
                                   seq1
                                   (filler length1 length2 fill-value))
                      seq1))
            (seq2 (if fill-value-p
                      (concatenate result-type
                                   seq2
                                   (filler length2 length1 fill-value))
                      seq2)))
        (map result-type #'list
             seq1 seq2)))))

(defun-ct maximizing (relation measure)
  (lambda (it)
    (let ((it-length (length it)))
      (when (> it-length 0)
        (values-list
         (reduce (lambda (|arg1764| |arg1765|)
                   (destructuring-bind (cur-max max-idx) |arg1764|
                     (destructuring-bind (next next-idx) |arg1765|
                       (if (funcall relation (funcall measure cur-max) (funcall measure next))
                           (list next next-idx)
                           (list cur-max max-idx)))))
                 (funcall (zipping 'vector)
                          it
                          (alexandria:iota it-length))))))))

(defun-ct group-by (fn &key (test 'equal))
  (lambda (seq)
    (let ((groups (make-hash-table :test test)))
      (map nil
           (lambda (it)
             (push it
                   (gethash (funcall fn it)
                            groups)))
           seq)
      (mapcar (lambda (it)
                (cons (car it)
                      (reverse (cdr it))))
              (alexandria:hash-table-alist groups)))))

#+nil
(defmacro <> (arity &rest funs)
  (let ((arg-syms (loop repeat arity collect (gensym))))
    `(lambda (,@arg-syms)
       (declare (dynamic-extent ,@arg-syms))
       ,(fw.lu:rollup-list (mapcar (lambda (x)
                                     (etypecase x
                                       (list `(funcall ,x))
                                       (symbol (list x))))
                                   funs)
                           arg-syms))))

(defmacro <>1 (&rest funs)
  `(alexandria:compose ,@funs))
(defmacro • (&rest funs)
  `(alexandria:compose ,@funs))
