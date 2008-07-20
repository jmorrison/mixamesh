
(in-package :cl-user)

(defpackage :mixamesh
  (:use :cl :iterate :cl-tuples)
  (:export #:vertex-array 
            #:face-array
            #:base-mesh
            #:mesh
            #:def-mesh-type             
            #:textured-mesh
            #:coloured-mesh
            #:faces-of
            #:vertices-of
            #:colours-of
            #:uvs-of
            #:*meshes*))

(in-package :mixamesh)




