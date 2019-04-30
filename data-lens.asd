(asdf:defsystem #:data-lens
  :description "Utilities for building data transormations from composable functions, modeled on lenses and transducers"
  :author "Edward Langley <edward@elangley.org>"
  :license "MIT"
  :depends-on (:cl-ppcre
               :alexandria
               :serapeum)
  :serial t
  :components ((:file "lens")))

