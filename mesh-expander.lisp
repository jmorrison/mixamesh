(in-package :mixamesh)


;; -- keep track of every mesh instance ----------------------------------

(defparameter *meshes* (make-hash-table :test 'eq)
  "A table of meshes.")

(defparameter *compiled-meshes* (make-hash-table :test 'eq)
  "A table of compiled meshes")

(defparameter *textures* (make-hash-table :test 'eq)
  "A table of textures.")



;; base mesh class type --------------------

(defclass base-mesh ()
  ((face-array :accessor faces-of :initform (make-triangle-array 0 :adjustable t :fill-pointer 0) :type (vector (unsigned-byte 16) *))
   (current-face-index :accessor current-face-index-of :initform 0 :type fixnum)
   (current-vertex-index :accessor current-vertex-index-of :initform 0 :type fixnum)
   (id :allocation :class :accessor id-of :initform (get-universal-time)))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Base class for all meshes"))

(defgeneric mesh-builder (mesh op &optional data) 
  (:method  ((mesh base-mesh) op &optional data)
    (case op
      (:face-add (triangle-vector-push-extend (triangle data) (faces-of mesh)) (triangle-array-dimensions (faces-of mesh)))
      (:face-set (setf (triangle-aref (faces-of mesh)  (the fixnum (current-face-index-of mesh))) (triangle data)))
      (:face-clear (setf (faces-of mesh) (make-triangle-array data :ajustable t :fill-pointer 0))))))



;; mesh - building protocol --------------------



;; constructor 
(defmethod initialize-instance :after ((self base-mesh) &rest args)
  (declare (ignore args))
  ;; treat the object as a function
  (setf (gethash (id-of self) *meshes*) self)
  (closer-mop:set-funcallable-instance-function 
   self
   #'(lambda (op &optional data) (mesh-builder self op data))))

(defun make-mesh (mesh-type)
  "Create a mesh instance and return the handle to it"
  (let* ((mesh
         (make-instance mesh-type))
        (result
         (id-of mesh)))
    (incf (id-of mesh))
    result))

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
  "Expands the clauses used to set an attribute indexed by the current vertex index."
  (declare (ignorable name))
  (loop
     for (array-name accessor-name type-name) in attributes
     collect
       `(,(cl-tuples::make-adorned-symbol array-name :prefix "SET" :package :keyword)
          (setf (,(cl-tuples::tuple-symbol type-name :def-tuple-aref)
                  (,accessor-name mesh)
                   (the fixnum (current-vertex-index-of mesh)))
                  (the ,(cl-tuples::tuple-element-type type-name) (,type-name data))))))

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

(defun expand-mesh-clearers (name attributes)
  "Expands the clauses use to clear attribute arrays in a mesh"
  (declare (ignorable name))
  (loop
     for (array-name accessor-name type-name) in attributes
     collect
       `(,(cl-tuples::make-adorned-symbol array-name :prefix "CLEAR" :package :keyword)
          (setf (,accessor-name mesh) 
                (,(cl-tuples::tuple-symbol type-name :def-tuple-array-maker) (list data) :adjustable t :fill-pointer 0)))))


(defun expand-mesh-builder-function (name attributes)
  "Expands the form used to deefine the function used to build the mesh"
  `(defmethod mesh-builder ((mesh ,name) op &optional data)
     (case op
       ,@(expand-mesh-setters name attributes)
       ,@(expand-mesh-clearers name attributes)
       ,@(expand-mesh-adders name attributes)       
       (:face-index (setf (current-face-index-of mesh) data))
       (:vertex-index (setf (current-vertex-index-of mesh) data))
       (otherwise (call-next-method)))))


(defun expand-attributes-list (attributes)
    (loop
      for (array-name accessor-name type-name) in attributes
      collect `(quote ,array-name)))
           
(defun expand-mesh-class  (name base slots attributes)
  "Expands the form used to declare a custom mesh class"
    `(defclass ,name (,@base)
       (,@slots
        ,@(expand-mesh-class-attributes attributes)
        (attributes :initform (list 'vertices 'faces ,@(expand-attributes-list attributes))  :reader attributes-of :allocation :class))   
       (:metaclass closer-mop:funcallable-standard-class)
       (:documentation "Custom mesh type")))

;; main mesh expansion macro --------------------

(defmacro def-mesh-type (name base &rest spec)
  (destructuring-bind (attributes &key (slots nil)) 
      spec
  `(progn ,(expand-mesh-class name base slots attributes)
          ,(expand-mesh-builder-function name attributes))))

