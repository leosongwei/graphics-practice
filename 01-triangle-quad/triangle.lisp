(load "../load-libs")

(progn
  (sdl2-ffi.functions:sdl-init sdl2-ffi:+sdl-init-video+)
  (sdl2-ffi.functions:sdl-gl-set-attribute sdl2-ffi:+sdl-gl-context-major-version+ 3)
  (sdl2-ffi.functions:sdl-gl-set-attribute sdl2-ffi:+sdl-gl-context-minor-version+ 3)
  (sdl2-ffi.functions:sdl-gl-set-attribute sdl2-ffi:+sdl-gl-context-profile-mask+
                                           sdl2-ffi:+sdl-gl-context-profile-core+))
(progn
  (defparameter *window*
    (cffi:with-foreign-string (title "test")
      (sdl2-ffi.functions:sdl-create-window
       title
       sdl2-ffi:+sdl-windowpos-undefined+ sdl2-ffi:+sdl-windowpos-undefined+
       640 480
       (logior sdl2-ffi:+sdl-window-opengl+ sdl2-ffi:+sdl-window-shown+))))

  (defparameter *glcontext*
    (sdl2-ffi.functions:sdl-gl-create-context *window*)))

(%gl:viewport 0 0 640 480)
(gl:clear-color 0.2 0.2 0.2 1.0)
(gl:clear :color-buffer-bit :depth-buffer-bit)
(sdl2-ffi.functions:sdl-gl-swap-window *window*)

(progn
  (defparameter *vertex-shader-string*
    "
#version 330 core
layout(location = 0) in vec3 vertexPosition_modelspace;
void main(){
    gl_Position.xyz = vertexPosition_modelspace;
    gl_Position.w = 1.0;
}
")

  (defparameter *fragment-shader-string*
    "
#version 330 core
out vec3 color;
void main(){
color = vec3(1,0,0); // red
}
"))

(let* ((vs (compile-shader-from-string :vertex-shader *vertex-shader-string*))
       (fs (compile-shader-from-string :fragment-shader *fragment-shader-string*)))
  (defparameter *shader-program* (create-program-with-shaders vs fs))
  (gl:use-program *shader-program*)
  (gl:delete-shader vs)
  (gl:delete-shader fs))

(defparameter *triangle-points*
  #(-1.0 -1.0  0.0
    1.0 -1.0  0.0
    0.0  1.0  0.0))

;; generate vao
(defparameter *vertex-array* (gl:gen-vertex-array))
(gl:bind-vertex-array *vertex-array*)

;; generate vbo
(defparameter *vertex-buffer* (gl:gen-buffer))
(gl:bind-buffer :array-buffer *vertex-buffer*)

;; send data to vbo
(with-float-buffer (buffer *triangle-points*)
  (%gl:buffer-data :array-buffer (* 4 (length *triangle-points*))
                   buffer :static-draw))

;; set shader vertex attrib pointer
(gl:enable-vertex-attrib-array 0)
(gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

(gl:bind-vertex-array *vertex-array*)
;;(gl:polygon-mode :front-and-back :line)
;;(gl:polygon-mode :front-and-back :fill)
(gl:draw-arrays :triangles 0 3)
(sdl2-ffi.functions:sdl-gl-swap-window *window*)

(read-char *standard-input* nil #\q)
(gl:use-program 0)
(gl:delete-program *shader-program*)
(exit)
