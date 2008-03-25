
(in-package :mixamesh)

(defclass material ()
  ((diffuse :accessor diffuse-color-of :initform (new-colour))
   (ambient :accessor ambient-color-of :initform (new-colour))
   (specular :accessor specular-color-of :initform (new-colour)))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Material associated with mesh polygons"))
