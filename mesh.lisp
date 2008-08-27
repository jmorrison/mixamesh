(in-package :mixamesh)

;; -- define a few common types of mesh for luck --------------------------

;; abstract base mesh 

;; simple mesh w vertices
(def-mesh-type simple-mesh (base-mesh) ((vertex vertices-of vertex3d)))

;; simple mesh with normals for lighting
(def-mesh-type mesh (simple-mesh) ((normal normals-of vector3d) (face-normals face-normals-of vector3d)))

;; mesh with textures
(def-mesh-type textured-mesh (simple-mesh) ((texcoord uvs-of vector2d)))

;; mesh with vertex colours or other attributes
(def-mesh-type coloured-mesh (simple-mesh) ((colour colours-of colour)))
 

;; -- operations on normals et al --------------------

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
           (vector3d* (+ ax bx) (+ ay by) (+ az bz))))

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

(defgeneric calc-face-normals (mesh))

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

(defgeneric calc-vertex-normals (mesh))

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
          (setf (vector3d-aref vertex-normals index) (vector3d normal)))))
    (setf (normals-of self) vertex-normals)))  


;; mesh geometry calculation ---------------------------------------------

(defgeneric box-of (mesh))

(defmethod box-of ((self simple-mesh))
  "Return a bounding box for the mesh."
  (let ((maxx most-negative-single-float)
        (minx most-positive-single-float)
        (maxy most-negative-single-float)
        (miny most-positive-single-float)
        (maxz most-negative-single-float)
        (minz most-positive-single-float))
    (iterate
      (for (values x y z w) in-vertices (vertices-of self))      
      (cond
        ((< x minx) (setf minx x))
        ((> x maxx) (setf maxx x))
        ((< y miny) (setf miny y))
        ((> y maxy) (setf maxy y))
        ((< z minz) (setf minz z))
        ((> z maxz) (setf maxz z))))
    (make-aabb* minx maxx miny maxy minz maxz)))

(defun bound-mesh (mesh)
  (setf (gethash mesh *bounding-boxes*) (box-of (gethash mesh *meshes*))))

;; squeeze into unit bounds (for brushes?) ---------------------------------

(defgeneric normalize-scale (mesh))

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
          (setf (vertex3d-aref (vertices-of self) index) 
                (vertex3d* (* x scale) (* y scale) (* z scale) w)))))))


;; to do -- this hierarchy should mirror the def-mesh hierarchy and
;; be automatically expanded out by def-mesh

;; compiled mesh -- methods defined elsehwhere
(defclass compiled-mesh ()
  ((vertex-buffer :initform 0 :type (unsigned-byte 32) :reader vertex-buffer-of)
   (triangle-buffer :initform 0 :type (unsigned-byte 32) :reader triangle-buffer-of)
   (element-count :initform 0 :type fixnum :reader element-count-of)))

(defclass textured-compiled-mesh ()
  ((vertex-buffer :initform 0 :type (unsigned-byte 32) :reader vertex-buffer-of)
   (uv-buffer :initform 0 :type (unsigned-byte 32) :reader uv-buffer-of)
   (texture :accessor texture-of)
   (element-count :initform 0 :type fixnum :reader element-count-of)))


(defgeneric stripify (mesh))

(defmethod stripify ((self mesh))
  "Stripify mesh")




