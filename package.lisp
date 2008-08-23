
(in-package :cl-user)

(defpackage :mixamesh
  (:use :cl :iterate :cl-tuples)
  (:export #:vertex-array 
            #:face-array
            #:base-mesh
            #:simple-mesh
            #:make-mesh
            #:mesh
            #:textured-mesh
            #:coloured-mesh            
            #:def-mesh-type             
            #:faces-of
            #:vertices-of
            #:colours-of
            #:uvs-of
            #:*textures*
            #:*meshes*
            #:*compiled-meshes*
            #:compile-mesh
            #:compiled-mesh
            #:element-count-of
            #:triangle-buffer-of
            #:vertex-buffer-of))

(in-package :mixamesh)




