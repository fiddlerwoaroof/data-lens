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

(defpackage :data-lens.transducers.beta
  (:use :cl)
  (:export #:mapping :filtering :deduping :catting :splitting
           #:exit-early :taking :dropping :transduce
           #:hash-table-builder :vector-builder :list-builder
           #:mv-mapping
           #:mv-selecting
           #:hash-table-select
           #:mv-filtering
           #:lazy-sequence))
