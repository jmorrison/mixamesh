(in-package :mixamesh)


;; base mesh class type --------------------


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

;; mesh - building protocol --------------------

(defgeneric mesh-builder (mesh op data))

;; constructor 
(defmethod initialize-instance :after ((self base-mesh) &rest args)
  (declare (ignore args))
  ;; treat the object as a function
  (closer-mop:set-funcallable-instance-function 
   self
   #'(lambda (op data) (mesh-builder self op data))))

;; expanders for individual aspects of a mesh --------------------

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

;; main mesh expansion macro --------------------

(defmacro def-mesh-type (name base &rest attributes)
  `(progn ,(expand-mesh-class name base attributes)
          ,(expand-mesh-builder-function name attributes)
          ,@(expand-mesh-predicates name attributes)
          ,@(expand-base-mesh-predicates name attributes)))

