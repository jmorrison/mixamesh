
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
;; (make-mesh-class wire-mesh
;;  (:attributes (:vertex vertex3d)
;;               (:colour color)))

;; (make-mesh-class :name textured-mesh
;;  (:attributes (:vertex vertex3d)
;;               (:colour color)
;;               (:texcoord texcoord)))


(defclass mesh-base ()
  ((vertex-index-array :accessor vertex-indices-of :documentation "Indices of triangle vertices" :initform nil)
   (current-vertex-index :accessor current-vertex-index-of :initform 0)
   (current-face-index :accessor current-face-index-of :initform 0))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Base mixin class for mesh"))

(defclass mesh (mesh-base)
  ((vertex-array :accessor vertices-of :initform nil)
   (current-vertex-index :accessor current-vertex-index-of :initform 0)
   (current-face-index :accessor current-face-index-of :initform 0))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Generic mesh type"))

(defun expand-mesh-class-attributes (attributes)
  (loop
     for attrib in attributes
     collect 
       `(,(cl-tuples::make-adorned-symbol (car  attrib) :suffix "ARRAY")
          :accessor ,(cl-tuples::make-adorned-symbol (car attrib) :suffix "OF")
          :initform nil)))

(defun expand-mesh-builder-setters (name attributes)
  (loop
     for attrib in attributes
     collect
       `(,(cl-tuples::make-adorned-symbol (car attrib) :prefix "SET" :package :keyword)
          (setf (,(cl-tuples::tuple-symboll :def-tuple-aref)
                  (,(cl-tuples::make-adorned-symbol (car attrib) :suffix "OF")
                    mesh)
                  (current-t-vertex-index of mesh) 
                  (,(cadr attrib) data))))))

(defun expand-mesh-builder-adders (name attributes)
  (loop
     for attrib in attributes
     collect
       `(,(cl-tuples::make-adorned-symbol (car attrib) :prefix "ADD" :package :keyword)
          )
))
(defun expand-mesh-builder-function (name attributes)
  `(defmethod mesh-builder ((mesh ,name) op data)
     (ecase op
          (:set-face 
           (setf (triangle-aref (vertex-indices-of mesh) (current-face-index-of mesh)) (triangle data)))
          
          
        )
       )
     ))

(defmacro def-mesh-class  (name &rest attributes)
    `(defclass ,name (mesh-base)
       (,@(expand-mesh-class-attributes attributes))   
       (:metaclass closer-mop-funcallable-standard-class)
       (:documentation "Custom mesh type")))

(defgeneric make-mesh-faces (mesh triangle-data))

;; mesh - building protocol
(defgeneric mesh-builder (mesh op data))

;; constructor : TO DO -- needs keys :has-vertices :has-normals :has-vetex-indices, etc
(defmethod initialize-instance :after ((self base-mesh) &key mesh)
  (when mesh
    (make-mesh self (car mesh) (cdr mesh))
    ;; treat the object as a function
    (closer-mop:set-funcallable-instance-function 
   self
   #'(lambda (op data) (mesh-builder self op data)))))


(defclass compiled-mesh (mesh)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Optimised, unmodifiable mesh"))

;; mesh building protocol
(defmethod mesh-builder ((mesh mesh) op data)
  (ecase op
    (:set-vertex 
     (setf (vertex3d-aref (vertices-of mesh) (current-vertex-index-of mesh)) (vertex3d data)))n
    (:set-face 
     (setf (triangle-aref (vertex-indices-of mesh) (current-face-index-of mesh)) (triangle data)))
    (:add-vertex 
     (progn
       (vertex3d-vector-push-extend (vertex3d data) (vertices-of mesh))
       (vertex3d-array-dimensions (vertices-of mesh))))
;;      (when (normals-of mesh)
;;        (vector3d-vector-push-extend data (normals-of mesh)))
;;      (when (colours-of mesh)
;;        (colour-vector-push-extend data (colours-of mesh)))
;;      (when (texcoords-of mesh)
;;        (vector2d-vector-push-exend data (texcoords-of mesh))))
    (:add-triangle
     (progn 
       (triangle-vector-push-extend (triangle data) (vertex-indices-of mesh))
       (triangle-array-dimensions (vertex-indices-of mesh))))
    
;;      (when (normal-indices-of mesh)
;;        (triangle-vector-push-extend data (normal-indices-of mesh)))
;;      (when (colour-indices-of mesh)
;;        (triangle-vector-push-extend data (colour-indices-of mesh)))
;;      (when (colour-indices-of mesh)
;;        (triangle-vector-push-extend data (colour-indices-of mesh)))
;;      (when (texcoords-indices-of mesh)
;;        (triangle-vector-push-extend data (texcoord-indices-of mesh)))
;;      (when (face-normals-of mesh)
;;        (vector3d-vector-push-extend data (face-normals-of mesh)))
    (:vertex-index (setf (current-vertex-index-of mesh) data))
    (:face-index (setf (current-face-index-of mesh) data))))


;; Topology, if we get clever.
;; (defclass half-edge ()
;;   ((vertex-of :initform 0 :documentation "Indexes vertex of he")
;;    (predecessor-of :documentation "Half edge previos to this one")
;;    (successor-of :documentation "Half edge next to this one")
;;    (tri-of :documentation "Face the he belongs to"))
;;   (:documentation "Half edge element"))

;; (:vertex () --) feeds elements into glVertex
;; (:normal () --) feeds elements into glNormal
;; (:colour () --) feeds elements into glColour
;; (:texcoord () --) feeds elements into glTexCoord
;; (:material (:ambient ) (:diffuse ) (:specular ))
;; (:triangle (:vertex () () () [(:normal ( ) () )]) ;; 
;;  -- indexes vertices by face, also other elements if present otherwise we assume they map to same indices as vertices

(defmethod make-mesh-faces ((self  mesh) triangle-data)
  "Fills in the index arrays in a mesh from the form supplied."
  (destructuring-bind
   (&key vertex normal colour texcoord) 
   triangle-data
   (assert vertex)
   (setf (vertex-indices-of self) (make-triangle-array (length vertex)))
   (iterate 
    (for (a b c) in vertex)
    (for index from 0 below (length vertex))
    (setf (triangle-aref (vertex-indices-of self) index) (values a b c)))
;;    (when normal
;;      (assert (= (length vertex) (length normal)))
;;      (setf (normal-indices-of self) (make-triangle-array (length vertex)))
;;      (iterate 
;;       (for (a b c) in normal)
;;       (for index from 0 below (length normal))
;;       (setf (triangle-aref (normal-indices-of self) index) (values a b c))))
;;    (when colour
;;      (assert (= (length vertex) (length colour)))
;;      (setf (colour-indices-of self) (make-triangle-array (length vertex)))
;;      (iterate 
;;       (for (a b c) in colour)
;;       (for index from 0 below (length colour))
;;       (setf (triangle-aref (colour-indices-of self) index) (values a b c))))
;;    (when texcoord
;;      (assert (= (length vertex) (length texcoord)))
;;      (setf (texcoord-indices-of self) (make-triangle-array (length vertex)))
;;      (iterate 
;;       (for (a b c) in texcoord)
;;       (for index from 0 below (length texcoord))
;;       (setf (triangle-aref (texcoord-indices-of self) index) (values a b c))))
))

;; (def-tuple-op calc-face-normal    
;;     ((vertex-a vertex3d (ax ay az aw))
;;      (vertex-b vertex3d (bx by bz bw))
;;      (vertex-c vertex3d (cx cy cz cw)))
;;   (vector3d-normal
;;    (vector3d-cross 
;;     (delta-vector3d vertex-a vertex-b)
;;     (delta-vector3d vertex-a vertex-c))))

;; (def-tuple-op vector3d-sum 
;;     ((vector-a vector3d (ax ay az))
;;      (vector-b vector3d (bx by bz)))
;;   (vector3d-tuple (+ ax bx) (+ ay by) (+ az bz)))

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


;; (defmethod calc-face-normals ((self mesh))
;;   "Calculate the face normals of a mesh."
;;   (let* ((face-normals (make-vector3d-array (triangle-array-dimensions (vertex-indices-of self)))))
;;     (iterate
;;       (for (values a b c) in-triangles (vertex-indices-of self))
;;       (for triangle-index upfrom 0)
;;       (setf (vector3d-aref face-normals triangle-index)
;;             (calc-face-normal 
;;              (vertex3d-aref (vertices-of self) a)
;;              (vertex3d-aref (vertices-of self) b)
;;              (vertex3d-aref (vertices-of self) c))))
;;     (setf (face-normals-of self) face-normals)))

;; (defmethod calc-vertex-normals ((self mesh))
;;   "Calculate the vertex normals of a mesh."
;;   (let ((vertex-normals (make-vector3d-array (length (vertices-of self)))))
;;     (iterate 
;;       (for index index-of-vertex (vertices-of self))
;;       (let ((normal (new-vector3d)))        
;;         (iterate
;;           (for (values a b c) in-triangles (vertex-indices-of self))
;;           (for face-index upfrom 0)
;;           (when (or (= a index) (= b index) (= c index))
;;             (setf (vector3d normal) 
;;                   (vector3d-sum (vector3d normal) 
;;                                 (vector3d-aref (face-normals-of self) face-index))))
;;           (setf (vector3d-aref vertex-normals  index) (vertex3d normal)))))
;;     (setf (normals-of self) vertex-normals)))


(defun make-mesh (name &rest args)
  "Create a mesh of the form (name :vertices (list of vertices) :normal (list of normals) :material (list of materials) :triangle (list of list of indices))  -- see make-mesh-triangles)"
  (labels 
      ((record-name ()
         (typecase name
           (symbol (import name :mesh-names))
           (string (intern name :mesh-names)))))
    (let ((mesh (make-instance 'mesh))
          (mesh-name (record-name)))
      (destructuring-bind 
            (&key vertices normals colours texcoords material indices)
          args
        (setf (vertices-of mesh) 
              (make-vertex3d-array (length vertices)))
        (iterate
          (for (x y z) in vertices)
          (for index from 0 below (length vertices))
          (setf (vertex3d-aref (vertices-of mesh)  index) (values x y z 1.0)))
;;         (when normals
;;           (setf (normals-of mesh)
;;                 (make-vector3d-array (length normals)))
;;           (iterate
;;             (for (x y z) in normals)
;;             (for i from 0 below (length normals))  
;;             (setf (vector3d-aref (normals-of mesh) i)  (values x y z))))
;;         (when colours
;;           (setf (colours-of mesh)
;;                 (make-colour-array (length colours)))
;;           (iterate
;;             (for (r g b) in colours)
;;             (for i from 0 below (length colours)) 
;;             (setf (vector3d-aref (colours-of mesh) i)  (values r g b))))
;;         (when texcoords
;;           (setf (texcoords-of mesh)
;;                 (make-vector2d-array (length texcoords)))
;;           (iterate
;;             (for (u v) in texcoords)
;;             (for i from 0 below (length vertices))
;;             (setf (vector2d-aref (texcoords-of mesh) i)  (values u v))))
        (when indices
          (make-mesh-faces mesh indices))
;;         (unless normals
;;           (setf (normals-of mesh)
;;                     (make-vector3d-array (length vertices)))))
      (setf (gethash mesh-name *meshes*) mesh)
      mesh-name))))



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


;; mesh rendering  ---------------------------------------------

;; (defmethod render ((self mesh))
;;   "Draw a mesh with any appropiate means."
;;   (iterate 
;;    (for (values x y z w) in-vertices (vertices-of mesh)
;;         (gl:vertex-3f x y z w))))

  


