
(in-package :asdf)

(defsystem :mixamesh
  :author "John Connors"
  :depends-on (:iterate 
                :closer-mop
                :cffi                 
                :cl-glfw 
                :cl-glfw-glu 
                :cl-glfw-opengl 
                :cl-glfw-opengl-version_1_1 
                :cl-glfw-opengl-version_1_2 
                :cl-tuples)
  :serial t
  :components
  ((:file "package")
   (:file "mesh")))