
(in-package :mixamesh)

(defparameter *meshes* (make-hash-table :test 'equalp)
  "A table of loaded meshes to use as brushes.")

(unless (find-package :mesh-names)
  (make-package :mesh-names))

(def-tuple-type triangle 
    :tuple-element-type (unsigned-byte 16) 
    :elements (a b c))

;; mesh definitons mirror ogl definitions


;; wishful


(defclass base-mesh ()
  ((id :reader id-of :initform (get-universal-time))
   (vertex-array :accessor vertices-of :initform (make-vertex3d-array 0 :adjustable t :fill-pointer 0))
   (face-array :accessor faces-of :initform (make-triangle-array 0 :adjustable t :fill-pointer 0))
   (current-vertex-index :accessor current-vertex-index-of :initform 0)
   (current-face-index :accessor current-face-index-of :initform 0))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Base mixin class for mesh"))

(defclass mesh (base-mesh)
  ((normals-array :accessor normals-of :initform (make-vector3d-array 0 :adjustable t :fill-pointer 0))   
   (face-normals :accessor face-normals-of :initform (make-vector3d-array 0 :adjustable t :fill-pointer 0)))
   (:metaclass closer-mop:funcallable-standard-class)
   (:documentation "Generic mesh type"))

(defmethod has-normal-attribute-p ((mesh mesh))
  t)

(defmethod has-color-attribute-p ((mesh mesh))
  nil)

(defmethod has-texcoord-attribute-p ((mesh mesh))
  nil)

(defun expand-mesh-class-attributes (attributes)
  "Expand the class slot defintion of the mesh attribute"
  (loop
     for (array-name accessor-name type-name) in attributes
     collect 
       `(,(cl-tuples::make-adorned-symbol array-name :suffix "ARRAY")
          :accessor ,accessor-name          
          :initform (,(cl-tuples::tuple-symbol type-name :def-tuple-array-maker) 0 :adjustable t :fill-pointer 0))))

(defun make-accessor-symbol (sym)
  "Given a symbol, suffix it with -OF so as to make an accessor name"
  (cl-tuples::make-adorned-symbol sym :suffix "OF"))

(defun expand-mesh-setters (name attributes)
  "Expands the clauses used to "
  (declare (ignorable name))
  (loop
     for (array-name accessor-name type-name) in attributes
     collect
       `(,(cl-tuples::make-adorned-symbol array-name :prefix "SET" :package :keyword)
          (setf (,(cl-tuples::tuple-symbol type-name :def-tuple-aref)
                  (,accessor-name mesh)
                  (current-vertex-index-of mesh)) 
                  (,type-name data)))))

(defun expand-mesh-adders (name attributes)
  "Expands the clauses used to add attribute values to a mesh."
  (declare (ignorable name))
  (loop
     for (array-name accessor-name type-name) in attributes
     collect
       `(,(cl-tuples::make-adorned-symbol array-name :prefix "ADD" :package :keyword)
          (,(cl-tuples::tuple-symbol type-name  :def-tuple-vector-push-extend) 
            (,(cl-tuples::tuple-symbol type-name :def-tuple-getter) data) 
            (,accessor-name mesh))
          (,(cl-tuples::tuple-symbol type-name :def-tuple-array-dimensions) (,accessor-name mesh)))))


(defun expand-mesh-builder-function (name attributes)
  "Expands the form used to deefine the function used to build the mesh"
  `(defmethod mesh-builder ((mesh ,name) op data)
     (case op
       (:set-vertex 
        (setf (vertex3d-aref (vertices-of mesh) (current-vertex-index-of mesh)) (vertex3d data)))
       (:set-face 
        (setf (triangle-aref (faces-of mesh) (current-face-index-of mesh)) (triangle data)))
       ,@(expand-mesh-setters name attributes)
       (:add-vertex 
        (vertex3d-vector-push-extend (vertex3d data) (vertices-of mesh))
        (vertex3d-array-dimensions (vertices-of mesh)))
       (:add-face
        (triangle-vector-push-extend (triangle data) (faces-of mesh))
        (triangle-array-dimensions (faces-of mesh)))
       ,@(expand-mesh-adders name attributes)
       (:face-index (setf (current-face-index-of mesh) data))
       (:vertex-index (setf (current-vertex-index-of mesh) data)))))


(defun expand-mesh-predicates (name attributes)
  "Expand predicate functions for finding out what attributes this mesh has."
  (iterate
       (for (array-name accessor-name type-name) in attributes)
       (collect `(defmethod ,(cl-tuples::make-adorned-symbol array-name :prefix "HAS" :suffix "ATTRIBUTE-P") ((mesh ,name))
                   t))))

(defun expand-base-mesh-predicates (name attributes)
  (declare (ignore name))
  (iterate
    (for (array-name accessor-name type-name) in attributes)
    (collect 
        `(defmethod ,(cl-tuples::make-adorned-symbol array-name :prefix "HAS" :suffix "ATTRIBUTE-P") ((mesh mesh))
           nil))))

(defun expand-mesh-class  (name base attributes)
  "Expands the form used to declare a custom mesh class"
    `(defclass ,name (,base)
       (,@(expand-mesh-class-attributes attributes))   
       (:metaclass closer-mop:funcallable-standard-class)
       (:documentation "Custom mesh type")))

(defmacro def-mesh-type (name base &rest attributes)
  `(progn ,(expand-mesh-class name base attributes)
          ,(expand-mesh-builder-function name attributes)
          ,@(expand-mesh-predicates name attributes)
          ,@(expand-base-mesh-predicates name attributes)))

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
   (vector3d-normal
    (vector3d-cross 
     (delta-vector3d vertex-a vertex-b)
     (delta-vector3d vertex-a vertex-c))))

(def-tuple-op vector3d-sum 
     ((vector-a vector3d (ax ay az))
      (vector-b vector3d (bx by bz)))
   (vector3d-tuple (+ ax bx) (+ ay by) (+ az bz)))

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
       (x y z)
       (setf (vertex3d-aref (vertices-of self) index) (vertex3d-tuple (* x scale) (* y scale) (* z scale) 1.0)))))))


(defmethod stripify ((self mesh))
  "Stripify mesh")

(defmethod decompilation ((self compiled-mesh))
  "Create a modifiable mesh from a compiled mesh")

(defmethod compilation ((self mesh))
  "Given a mesh return a compiled mesh, which is a non-modifiable mesh optimised for rendering in foreign memory."
)


