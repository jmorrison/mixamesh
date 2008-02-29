
(in-package :cl-user)

(defpackage :mixamesh
  (:use (:iterate :cl-tuples)
   :export (#:compilation 
            #:deindex
            #:stripify
            #:decompilation 
            #:render
            #:mesh              
            #:compiled-mesh
            #:make-mesh
            #:*meshes*)))




