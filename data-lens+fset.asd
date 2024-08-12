(in-package :asdf-user)

(asdf:defsystem #:data-lens+fset
  :author "Edward Langley <el-cl@elangley.org>"
  :license "Apache v2"
  :depends-on (:data-lens
               :data-lens/transducers
               :fset
               :named-readtables)
  :serial t
  :in-order-to ((test-op (test-op :data-lens/test)))
  :components ((:file "data-lens-fset")))
