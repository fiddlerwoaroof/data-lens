(in-package :asdf-user)

(asdf:defsystem #:data-lens
  :description #.(format nil "~@{~a~^ ~}"
                         "Utilities for building data transformations from"
                         "composable functions, modeled on lenses and"
                         "transducers")
  :author "Edward Langley <el-cl@elangley.org>"
  :license "Apache v2"
  :depends-on (:cl-ppcre
               :alexandria
               (:require :sb-cover))
  :serial t
  :in-order-to ((test-op (test-op :data-lens/test)))
  :components ((:file "package")
               (:file "optics")
               (:file "lens")))

(asdf:defsystem #:data-lens/test
  :description "tests for the transducers"
  :author "Edward Langley <el-cl@elangley.org>"
  :license "Apache v2"
  :depends-on (:data-lens
               :fiveam
               :string-case)
  :serial t
  :perform (test-op (o c)
                    (unless (symbol-call :fiveam '#:run! :data-lens.lens)
                      (error "some tests failed")))
  :components ((:module "t"
                :components ((:file "lens")))))

(asdf:defsystem #:data-lens/beta/transducers
  :description #.(format nil "~@{~a~^ ~}"
                         "A collection of transducers to reduce stream-manipulation overhead")
  :author "Edward Langley <el-cl@elangley.org>"
  :license "Apache v2"
  :depends-on (:data-lens
               :alexandria
               :serapeum)
  :serial t
  :in-order-to ((test-op (test-op :data-lens/transducers/test)))
  :components ((:file "package")
               (:file "transducer-protocol")
               (:file "transducers")
               (:file "lazy-sequence")))

(asdf:defsystem #:data-lens/transducers/test
  :description "tests for the transducers"
  :author "Edward Langley <el-cl@elangley.org>"
  :license "Apache v2"
  :depends-on (:data-lens/beta/transducers
               :fiveam)
  :serial t
  :perform (test-op (o c) (unless (symbol-call :fiveam '#:run! :data-lens.transducers)
                            (error "some tests failed")))
  :components ((:module "t"
                :components ((:file "transducers")))))
