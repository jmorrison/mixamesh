
(in-package :asdf)

(defsystem :mixamesh
  :author "John Connors"
  :depends-on (:iterate :cl-tuples)
  :serial t
  :components
  ((:file "package")
   (:file "mesh")))