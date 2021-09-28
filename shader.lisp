(defun glenum-number (keyword)
  (cffi:foreign-enum-value 'cl-opengl-bindings::enum keyword))

(defun compile-shader-from-string (shader-type lisp-string)
  "=> shader id"
  (let ((shader-id (gl:create-shader shader-type)))
    (if (= 0 shader-id)
        (error "compile-shader-from-string: Unable to create shader"))
    (gl:shader-source shader-id (list lisp-string))
    (gl:compile-shader shader-id)
    (if (not (gl:get-shader shader-id :compile-status))
        (let ((log (gl:get-shader-info-log shader-id)))
          (format t "Shader compile error:~%")
          (format t "~A~%" log)
          (error "compile-shader-from-string: shader compile failed")))
    shader-id))

;; (compile-shader-from-string :vertex-shader *vertex-shader-string*)
;; (cffi:foreign-enum-keyword-list 'cl-opengl-bindings::shader-type)
;; ==> (:COMPUTE-SHADER :FRAGMENT-SHADER :FRAGMENT-SHADER-ARB :GEOMETRY-SHADER
;;      :TESS-CONTROL-SHADER :TESS-EVALUATION-SHADER :VERTEX-SHADER :VERTEX-SHADER-ARB)

(defun create-program-with-shaders (vs fs)
  "=> program id"
  (let ((program-id (gl:create-program)))
    (gl:attach-shader program-id vs)
    (gl:attach-shader program-id fs)
    (gl:link-program program-id)
    (gl:detach-shader program-id vs)
    (gl:detach-shader program-id fs)
    (if (not (gl:get-program program-id :link-status))
        (let ((log (gl:get-program-info-log program-id)))
          (format t "Program link error:~%")
          (format t "~A~%" log)
          (error "create-program-with-shaders: program link failed")))
    program-id))

;; (create-program-with-shaders (compile-shader-from-string :vertex-shader *vertex-shader-string*)
;;                              (compile-shader-from-string :fragment-shader *fragment-shader-string*))
