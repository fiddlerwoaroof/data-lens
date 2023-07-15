(defpackage :data-lens.package
  (:use :cl )
  (:export ))
(in-package :data-lens.package)

(defpackage :data-lens.lenses
  (:shadow :set)
  (:use :cl)
  (:export #:over #:set #:view #:make-alist-lens #:make-plist-lens
           #:make-hash-table-lens #:make-list-lens))

(defpackage :data-lens
  (:use :cl)
  (:export #:regex-match #:include #:exclude #:pick #:key-transform
           #:combine #:derive #:cumsum #:over #:on #:shortcut
           #:defun-ct #:key #:extract-key #:element #:let-fn #:juxt
           #:transform-tail #:slice #:compress-runs
           #:combine-matching-lists #:sorted #:applicable-when
           #:of-length #:of-min-length #:of-max-length #:transform-head
           #:maximizing #:zipping #:applying #:splice-elt
           #:transform-elt #:denest #:op #:defalias #:<> #:<>1 #:== #:•
           #:∘ #:suffixp #:functionalize #:inc #:group-by #:keys
           #:conj #:disj #:delay #:of-type #:transform))

(defpackage :data-lens.transducers.internals
  (:use :cl)
  (:export #:unwrap #:init #:reduce-generic #:stepper #:transduce
           #:exit-early #:into #:builder-for-input))

(defpackage :data-lens.transducers
  (:use :cl)
  (:import-from #:data-lens.transducers.internals #:unwrap #:init
                #:reduce-generic #:stepper #:transduce #:exit-early
                #:into)
  (:export #:mapping #:filtering #:deduping #:catting #:splitting
           #:exit-early #:taking #:dropping #:transduce
           #:hash-table-builder #:vector-builder #:list-builder
           #:collecting #:mv-mapping #:mv-selecting #:hash-table-select
           #:mv-filtering #:mapcatting #:lazy-sequence
           #:compressing-runs #:iota #:repeating #:repeating* #:into))
