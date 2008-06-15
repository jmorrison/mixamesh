
(in-package :mixamesh)

(defparameter *meshes* (make-hash-table :test 'equalp)
  "A table of loaded meshes to use as brushes.")

(unless (find-package :mesh-names)
  (make-package :mesh-names))


(def-mesh-type wire-mesh base-mesh (colours colours-of colour))
(def-mesh-type textured-mesh mesh (texcoord uvs-of vector2d))
(def-mesh-type coloured-mesh mesh (colour colours-of colour))
  

;; mesh - building protocol
(defgeneric mesh-builder (mesh op data))

;; constructor 
(defmethod initialize-instance :after ((self base-mesh) &rest args)
  (declare (ignore args))
  ;; treat the object as a function
  (closer-mop:set-funcallable-instance-function 
   self
   #'(lambda (op data) (mesh-builder self op data))))



(def-tuple-op calc-face-normal    
     ((vertex-a vertex3d (ax ay az aw))
      (vertex-b vertex3d (bx by bz bw))
      (vertex-c vertex3d (cx cy cz cw)))
  "return the normal of a face"
  (:return vector3d
           (vector3d-normal
            (vector3d-cross 
             (delta-vector3d vertex-a vertex-b)
             (delta-vector3d vertex-a vertex-c)))))

(def-tuple-op vector3d-sum 
     ((vector-a vector3d (ax ay az))
      (vector-b vector3d (bx by bz)))
  "return the sum of two vectors"
  (:return vector3d
           (vector3d-tuple (+ ax bx) (+ ay by) (+ az bz))))

;; declarations to allow the iterate macro to iterate over the
;; triangles in a mesh or the vertices
(defclause-sequence in-triangles index-of-triangle 
  :access-fn 'triangle-aref
  :size-fn 'triangle-array-dimensions
  :sequence-type 'vector
  :element-type '(values  (unsigned-byte 16) (unsigned-byte 16) (unsigned-byte 16)))


(defclause-sequence in-vertices index-of-vertex
  :access-fn 'vertex3d-aref
  :size-fn 'vertex3d-array-dimensions
  :sequence-type 'vector
  :element-type '(values (unsigned-byte 16) (unsigned-byte 16) (unsigned-byte 16) (unsigned-byte 16)))


(defmethod calc-face-normals ((self mesh))
   "Calculate the face normals of a mesh."
   (let* ((face-normals (make-vector3d-array (triangle-array-dimensions (faces-of self)))))
     (iterate
       (for (values a b c) in-triangles (faces-of self))
       (for triangle-index upfrom 0)
       (setf (vector3d-aref face-normals triangle-index)
             (calc-face-normal 
              (vertex3d-aref (vertices-of self) a)
              (vertex3d-aref (vertices-of self) b)
              (vertex3d-aref (vertices-of self) c))))
     (setf (face-normals-of self) face-normals)))

(defmethod calc-vertex-normals ((self mesh))
  "Calculate the vertex normals of a mesh."
  (let ((vertex-normals (make-vector3d-array (length (vertices-of self)))))
    (iterate 
      (for index index-of-vertex (vertices-of self))
      (let ((normal (new-vector3d)))        
        (iterate
          (for (values a b c) in-triangles (faces-of self))
          (for face-index upfrom 0)
          (when (or (= a index) (= b index) (= c index))
            (setf (vector3d normal) 
                  (vector3d-sum (vector3d normal) 
                                (vector3d-aref (face-normals-of self) face-index))))
          (setf (vector3d-aref vertex-normals  index) (vertex3d normal)))))
    (setf (normals-of self) vertex-normals)))


;; mesh geometry calculation ---------------------------------------------

(defmethod box-of ((self mesh))
  "Return a bounding box for the mesh."
  (let ((maxx most-negative-single-float)
        (minx most-positive-single-float)
        (maxy most-negative-single-float)
        (miny most-positive-single-float)
        (maxz most-negative-single-float)
        (minz most-positive-single-float))
    (iterate
      (for index index-of-vertex (vertices-of self))      
      (with-vertex3d    
       (vertex3d-aref (vertices-of self) index)
       (x y z w)
       (cond
        ((< x minx) (setf minx x))
        ((> x maxx) (setf maxx x))
        ((< y miny) (setf miny y))
        ((> y maxy) (setf maxy y))
        ((< z minz) (setf minz z))
        ((< z minz) (setf minz z)))))
    (values minx maxx miny maxy minz maxz)))

(defmethod normalize-scale ((self mesh))
  "Rescale geometry to fit into a 1:1:1 bounding box"
  (multiple-value-bind
        (minx maxx miny maxy minz maxz)
      (box-of self)
    (let ((dx (- maxx minx))
          (dy (- maxy miny))
          (dz (- maxz minz))
        (scale))
    (cond
      ((and  (> dx dz) (> dx dy)) 
       ;; dx is largest dimension
       (setf scale (/ 1 (- maxx minx))))
      ;; dy is largest dimension
      ((and (> dy dz) (> dy dx))  
       (setf scale (/ 1 (- maxy miny))))
      ;; dz is largest dimension
      ((and (> dz dy) (> dz dx))
       (setf scale (/ 1 (- maxy miny)))))
    (iterate 
      (for index index-of-vertex (vertices-of self))
      (with-vertex3d 
       (vertex3d-aref (vertices-of self) index)
       (x y z w)
       (setf (vertex3d-aref (vertices-of self) index) (vertex3d-tuple (* x scale) (* y scale) (* z scale) w)))))))


(defmethod stripify ((self mesh))
  "Stripify mesh")

;; (defmethod decompilation ((self compiled-mesh))
;;   "Create a modifiable mesh from a compiled mesh")

;; (defmethod compilation ((self mesh))
;;   "Given a mesh return a compiled mesh, which is a non-modifiable mesh optimised for rendering in foreign memory."
;; )


