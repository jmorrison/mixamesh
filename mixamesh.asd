
(in-package :asdf)

(defsystem :mixamesh
  :author "John Connors"
  :depends-on (:iterate 
                :closer-mop
                :cl-tuples)
  :serial t
  :components
  ((:file "package")
   (:file "mesh-expander")
   (:file "mesh")))