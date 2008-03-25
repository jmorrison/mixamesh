
(in-package :cl-user)

(defpackage :mixamesh
  (:use :cl :iterate :cl-tuples)
  (:export #:compilation 
            #:deindex
            #:stripify
            #:decompilation 
            #:mesh              
            #:compiled-mesh
            #:make-mesh
            #:*meshes*))

(in-package :mixamesh)




