(defpackage :data-lens.package
  (:use :cl )
  (:export ))
(in-package :data-lens.package)

(defpackage :data-lens.lenses
  (:shadow :set)
  (:use :cl)
  (:export :over :set :view :make-alist-lens :make-plist-lens :make-hash-table-lens
           :make-list-lens))

(defpackage :data-lens
  (:use :cl)
  (:import-from #:serapeum #:op #:defalias)
  (:export #:regex-match #:include #:exclude #:pick #:key-transform
           #:combine #:derive #:cumsum #:over #:on #:shortcut #:defun-ct #:key
           #:extract-key #:element #:let-fn #:juxt #:transform-tail #:slice
           #:compress-runs #:combine-matching-lists #:sorted #:applicable-when
           #:of-length #:of-min-length #:of-max-length #:transform-head
           #:maximizing #:zipping #:applying #:splice-elt #:transform-elt #:denest
           #:op #:defalias #:<> #:<>1 #:== #:•
           ))
