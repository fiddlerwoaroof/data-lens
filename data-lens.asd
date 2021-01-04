(asdf:defsystem #:data-lens
  :description #.(format nil "~@{~a~^ ~}"
                         "Utilities for building data transformations from"
                         "composable functions, modeled on lenses and"
                         "transducers")
  :author "Edward Langley <el-cl@elangley.org>"
  :license "MIT"
  :depends-on (:cl-ppcre
               :alexandria
               :serapeum)
  :serial t
  :components ((:file "package")
               (:file "optics")
               (:file "lens")))

(asdf:defsystem #:data-lens/beta/transducers
  :description #.(format nil "~@{~a~^ ~}"
                         "A collection of transducers to reduce stream-manipulation overhead")
  :author "Edward Langley <el-cl@elangley.org>"
  :license "MIT"
  :depends-on (:data-lens
               :alexandria)
  :serial t
  :in-order-to ((test-op (test-op :data-lens/transducers/test)))
  :components ((:file "package")
               (:file "transducer-protocol")
               (:file "transducers")
               (:file "lazy-sequence")))

(asdf:defsystem #:data-lens/transducers/test
  :description "tests for the transducers"
  :author "Edward Langley <el-cl@elangley.org>"
  :license "MIT"
  :depends-on (:data-lens/beta/transducers
               :fiveam)
  :serial t
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :data-lens.transducers))
  :components ((:module "t"
                :components ((:file "transducers")))))
