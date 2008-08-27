
(in-package :mixamesh)

(defparameter *textures* (make-hash-table :test 'eql)
  "A table of textures.")

(defclass material ()
  ((diffuse :accessor diffuse-color-of :initform (new-colour))
   (ambient :accessor ambient-color-of :initform (new-colour))
   (specular :accessor specular-color-of :initform (new-colour)))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Material associated with mesh polygons"))

(defclass texture ()
  ((id :allocation :class :reader id-of :initform (get-universal-time))
   (width :accessor width-of :initarg :width)
   (height :accessor height-of :initarg :height)
   (map :accessor map-of :initarg :map)))
  

  
  
(defun make-texture (&key width height colour-map)
  (let* ((texture
          (make-instance 'texture :width width :height height :map colour-map))
         (result
          (id-of texture)))
    (setf (gethash (id-of texture) *textures*) texture)
    (incf (slot-value texture 'id))
    result))