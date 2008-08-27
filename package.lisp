
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
            #:make-texture
            #:width-of
            #:height-of
            #:map-of
            #:*meshes*
            #:*compiled-meshes*
            #:compile-mesh
            #:compiled-mesh
            #:textured-compiled-mesh
            #:element-count-of
            #:triangle-buffer-of
            #:vertex-buffer-of
            #:*bounding-boxes*
            #:bound-mesh
            #:uv-buffer-of
            #:box-of
            #:material
            #:texture
            #:texture-of))

(in-package :mixamesh)




