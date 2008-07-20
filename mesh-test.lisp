;; lets try creating and viewing some platonic solids
(require '#:asdf)
(asdf:oos 'asdf:load-op '#:cl-glfw)
(asdf:oos 'asdf:load-op '#:cl-glfw-opengl)
(asdf:oos 'asdf:load-op '#:cl-glfw-glu)
(asdf:oos 'asdf:load-op '#:mixamesh)

(in-package :mixamesh)


(defun make-octahedron ()
 (let ((result (make-instance 'wire-mesh)))
       (funcall result :add-vertex (make-vertex3d*  1.0  0.0  0.0 1.0))
       (funcall result :add-vertex (make-vertex3d* -1.0  0.0  0.0 1.0))
       (funcall result :add-vertex (make-vertex3d*  0.0  1.0  0.0 1.0))
       (funcall result :add-vertex (make-vertex3d*  0.0 -1.0  0.0 1.0))
       (funcall result :add-vertex (make-vertex3d*  0.0  0.0  1.0 1.0))
       (funcall result :add-vertex (make-vertex3d*  0.0  0.0 -1.0 1.0))
       (funcall result :add-face (make-triangle* 0 2 4))
       (funcall result :add-face (make-triangle* 2 0 5))
       (funcall result :add-face (make-triangle* 4 0 4))
       (funcall result :add-face (make-triangle* 0 3 5))
       (funcall result :add-face (make-triangle* 2 1 4))
       (funcall result :add-face (make-triangle* 1 2 5))
       (funcall result :add-face (make-triangle* 1 4 4))
       (funcall result :add-face (make-triangle* 3 1 5))
       result))     

(defun make-tetrahedron ()
  (let ((result (make-instance 'wire-mesh)))
    (funcall result :add-vertex (make-vertex3d*  1.0  1.0  1.0 1.0))
    (funcall result :add-vertex (make-vertex3d*  1.0 -1.0 -1.0 1.0))
    (funcall result :add-vertex (make-vertex3d* -1.0  1.0 -1.0 1.0))
    (funcall result :add-vertex (make-vertex3d* -1.0 -1.0  1.0 1.0))
    (funcall result :add-face (make-triangle* 3 2 1))
    (funcall result :add-face (make-triangle* 2 3 0))
    (funcall result :add-face (make-triangle* 1 0 3))
    (funcall result :add-face (make-triangle* 0 1 2))
    result))


(defun write-value (stream mesh)
  (format stream "Faces~%")
  (iterate
    (for (values a b c) in-triangles (faces-of mesh))
    (format stream "A ~A B ~A C ~A~%" a b c))
  (format stream "Vertices~%")
  (iterate
    (for (values x y z w) in-vertices (vertices-of mesh))
    (format stream "X ~A Y ~A Z ~A W ~A~%" x y z w)))


(defun render (mesh)
  ;; wireframe renderer
  (gl:with-begin gl:+line-loop+
    (gl:color-3f 1.0 1.0 1.0)
    (iterate 
      (for (values  a b c) in-triangles (faces-of mesh))    
      (with-vertex3d 
          (vertex3d-aref (vertices-of mesh) a) 
          (x y z w)
        (gl:vertex-3f x y z))
      (with-vertex3d 
          (vertex3d-aref (vertices-of mesh) b) 
          (x y z w)
        (gl:vertex-3f x y z))
      (with-vertex3d 
          (vertex3d-aref (vertices-of mesh) c) 
          (x y z w)
        (gl:vertex-3f x y z)))))

(defun mixamesh-test ()
    (let ((frames 0)
          (cube (make-cube))
          t0 
          t1)
      (glfw:do-window ("Mixamesh Test" 640 480)
          ((glfw:enable glfw:+sticky-keys+)
           (glfw:swap-interval 0)
           (setf t0 (glfw:get-time)
                 t1 (glfw:get-time)))

        (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
          (return-from glfw:do-window))

        (setf t1 (glfw:get-time))

        (when (or (> (- t1 t0) 1)
                  (= frames 0))
          (glfw:set-window-title (format nil "Mixamesh test (~,1f FPS)" (/ frames (- t1 t0))))
          (setf frames 0
                t0 t1))

        (incf frames)

        (destructuring-bind (width height) (glfw:get-window-size)
          (setf height (max height 1))
          (gl:viewport 0 0 width height)

          (gl:clear-color 0 0 0 0)
          (gl:clear gl:+color-buffer-bit+)

          (gl:matrix-mode gl:+projection+)
          (gl:load-identity)
          (glu:perspective 65 (/ width height) 1 100)
          (gl:matrix-mode gl:+modelview+)
          (gl:load-identity)
          (glu:look-at 4.5  4.50 1.0
                       4.5  4.5 20.0
                       0  1 0)                  
          (render cube)))))
