
(in-package :cl-user)

(defpackage :mixamesh
  (:use :cl :iterate :cl-tuples)
  (:export #:vertex-array 
            #:face-array
            #:base-mesh
            #:simple-mesh
            #:mesh
            #:textured-mesh
            #:coloured-mesh            
            #:def-mesh-type             
            #:faces-of
            #:vertices-of
            #:colours-of
            #:uvs-of
            #:*meshes*))

(in-package :mixamesh)




