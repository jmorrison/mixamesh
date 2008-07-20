(defpackage :mixamesh-tests
    (:use :cl :iterate :cl-tuples :mixamesh))

(in-package :mixamesh-tests)


;; create platonic solids

(def-mesh-type wire-mesh (mesh)  ())

(defun make-cube ()
  (let ((result (make-instance 'wire-mesh)))
	(funcall result :add-vertex (make-vertex3d*  4.0 4.0 4.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  5.0 4.0 4.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  5.0 5.0 4.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  4.0 5.0 4.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  5.0 4.0 5.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  5.0 5.0 5.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  4.0 5.0 5.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  4.0 4.0 5.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  4.0 5.0 5.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  4.0 5.0 4.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  5.0 4.0 5.0 1.0))
	(funcall result :add-vertex (make-vertex3d*  5.0 4.0 4.0 1.0))
    (funcall result :add-face   (make-triangle* 0 2 1))
    (funcall result :add-face   (make-triangle* 0 3 2))
    (funcall result :add-face   (make-triangle* 1 5 4))
    (funcall result :add-face   (make-triangle* 1 2 5))
    (funcall result :add-face   (make-triangle* 4 6 7))
    (funcall result :add-face   (make-triangle* 4 5 6))
    (funcall result :add-face   (make-triangle* 7 3 0))
    (funcall result :add-face   (make-triangle* 7 6 3))
    (funcall result :add-face   (make-triangle* 9 5 2))
    (funcall result :add-face   (make-triangle* 9 8 5))
    (funcall result :add-face   (make-triangle* 0 11 10))
    (funcall result :add-face   (make-triangle* 0 10 7))
    result))

(defun make-tetrahedron ()
  (let ((result (make-instance 'wire-mesh)))
    (funcall result :add-vertex (make-vertex3d* 0.0 -0.525731 0.850651 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.850651 0.0 0.525731 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.850651 0.0 -0.525731 1.0))
    (funcall result :add-vertex (make-vertex3d* -0.850651 0.0 -0.525731 1.0))
    (funcall result :add-vertex (make-vertex3d* -0.850651 0.0 0.525731 1.0))
    (funcall result :add-vertex (make-vertex3d* -0.525731 0.850651 0.0 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.525731 0.850651 0.0 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.525731 -0.850651 0.0 1.0))
    (funcall result :add-vertex (make-vertex3d* -0.525731 -0.850651 0.0 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.0 -0.525731 -0.850651 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.0 0.525731 -0.850651 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.0 0.525731 0.850651 1.0))
    (funcall result :add-face (make-triangle* 6 2 1))
    (funcall result :add-face (make-triangle* 2 7 1))
    (funcall result :add-face (make-triangle* 5 4 3))
    (funcall result :add-face (make-triangle* 8 3 4))
    (funcall result :add-face (make-triangle* 11 5 6))
    (funcall result :add-face (make-triangle* 10 6 5))
    (funcall result :add-face (make-triangle* 2 10 9))
    (funcall result :add-face (make-triangle* 3 9 10))
    (funcall result :add-face (make-triangle* 9 8 7))
    (funcall result :add-face (make-triangle* 0 7 8))
    (funcall result :add-face (make-triangle* 1 0 11))
    (funcall result :add-face (make-triangle* 4 11 0))
    (funcall result :add-face (make-triangle* 10 2 6))
    (funcall result :add-face (make-triangle* 11 6 1))
    (funcall result :add-face (make-triangle* 10 5 3))
    (funcall result :add-face (make-triangle* 11 4 5))
    (funcall result :add-face (make-triangle* 9 7 2))
    (funcall result :add-face (make-triangle* 0 1 7))
    (funcall result :add-face (make-triangle* 8 9 3))
    (funcall result :add-face (make-triangle* 0 8 4))
    result))

(defun make-icosahedron ()
  (let ((result (make-instance 'wire-mesh)))
    (funcall result :add-vertex (make-vertex3d* 0.0 -0.525731 0.850651 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.850651 0.0 0.525731 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.850651 0.0 -0.525731 1.0))
    (funcall result :add-vertex (make-vertex3d* -0.850651 0.0 -0.525731 1.0))
    (funcall result :add-vertex (make-vertex3d* -0.850651 0.0 0.525731 1.0))
    (funcall result :add-vertex (make-vertex3d* -0.525731 0.850651 0.0 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.525731 0.850651 0.0 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.525731 -0.850651 0.0 1.0))
    (funcall result :add-vertex (make-vertex3d* -0.525731 -0.850651 0.0 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.0 -0.525731 -0.850651 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.0 0.525731 -0.850651 1.0))
    (funcall result :add-vertex (make-vertex3d* 0.0 0.525731 0.850651 1.0))
    (funcall result :add-face (make-triangle* 6 2 1 ))
    (funcall result :add-face (make-triangle* 2 7 1 ))
    (funcall result :add-face (make-triangle* 5 4 3 ))
    (funcall result :add-face (make-triangle* 8 3 4 ))
    (funcall result :add-face (make-triangle* 11 5 6 ))
    (funcall result :add-face (make-triangle* 10 6 5 ))
    (funcall result :add-face (make-triangle* 2 10 9 ))
    (funcall result :add-face (make-triangle* 3 9 10 ))
    (funcall result :add-face (make-triangle* 9 8 7 ))
    (funcall result :add-face (make-triangle* 0 7 8 ))
    (funcall result :add-face (make-triangle* 1 0 11 ))
    (funcall result :add-face (make-triangle* 4 11 0 ))
    (funcall result :add-face (make-triangle* 10 2 6 ))
    (funcall result :add-face (make-triangle* 11 6 1 ))
    (funcall result :add-face (make-triangle* 10 5 3 ))
    (funcall result :add-face (make-triangle* 11 4 5 ))
    (funcall result :add-face (make-triangle* 9 7 2 ))
    (funcall result :add-face (make-triangle* 0 1 7 ))
    (funcall result :add-face (make-triangle* 8 9 3 ))
    (funcall result :add-face (make-triangle* 0 8 4 ))    
    result))

(defparameter *platonics*
  (list (make-cube) (make-tetrahedron) (make-icosahedron)))

;; iterate over vertices and faces

(defun write-mesh (stream mesh)
  (format stream "Faces~%")
  (iterate
    (for (values a b c) in-triangles (mixamesh::faces-of mesh))
    (format stream "A ~A B ~A C ~A~%" a b c))
  (format stream "Vertices~%")
  (iterate
    (for (values x y z w) in-vertices (mixamesh::vertices-of mesh))
    (format stream "X ~A Y ~A Z ~A W ~A~%" x y z w)))

(mapcar #'(lambda (m) (write-mesh *standard-output* m)) *platonics*)