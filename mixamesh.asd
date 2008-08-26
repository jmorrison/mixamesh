
(defpackage :mixamesh-system
    (:use :cl :asdf))

(in-package :mixamesh-system)

(defsystem :mixamesh
  :author "John Connors"
  :depends-on (:iterate 
                :closer-mop
                :cl-tuples)
  :serial t
  :components
  ((:file "package")
   (:file "mesh-expander")
   (:file "mesh")
   (:file "material")))

(defsystem :mixamesh-tests
  :author "John Connors"
  :depends-on (:mixamesh)
  :serial t
  :components
  ((:file "mixamesh-tests")))