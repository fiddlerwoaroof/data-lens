(asdf:defsystem #:data-lens
  :description #.(format nil "~a ~a ~a"
                         "Utilities for building data transormations from"
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
