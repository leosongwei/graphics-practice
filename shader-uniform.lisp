(load "load-libs")

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
layout(location = 0) in vec2 vertexPosition_modelspace;
void main(){
    gl_Position.xy = vertexPosition_modelspace;
    gl_Position.z = 0.0;
    gl_Position.w = 1.0;
}
")

  (defparameter *fragment-shader-string*
    "
#version 330 core
out vec3 color;

uniform float ourColor;

void main(){
    color.g = ourColor;
    color.r = 0.2;
    color.b = 0.2;
}
"))

(let* ((vs (compile-shader-from-string :vertex-shader *vertex-shader-string*))
       (fs (compile-shader-from-string :fragment-shader *fragment-shader-string*)))
  (defparameter *shader-program* (create-program-with-shaders vs fs))
  (gl:use-program *shader-program*)
  (gl:delete-shader vs)
  (gl:delete-shader fs))

(defparameter *quad-points*
  #(-0.5 0.5
    0.5 0.5
    -0.5 -0.5
    0.5 -0.5))

;; generate vao
(defparameter *vertex-array* (gl:gen-vertex-array))
(gl:bind-vertex-array *vertex-array*)

;; generate vbo
(defparameter *vertex-buffer* (gl:gen-buffer))
(gl:bind-buffer :array-buffer *vertex-buffer*)

;; send data to vbo
(with-float-buffer (buffer *quad-points*)
  (%gl:buffer-data :array-buffer (* 4 (length *quad-points*))
                   buffer :static-draw))

;; EBO points
(defparameter *ebo-indicies* #(0 3 1 0 2 3))
(defparameter *ebo* (gl:gen-buffer))
(gl:bind-buffer :element-array-buffer *ebo*)
(with-c-buffer (buffer *ebo-indicies* :uint32)
  (%gl:buffer-data :element-array-buffer (* 4 (length *ebo-indicies*))
                  buffer :static-draw))

;; set shader vertex attrib pointer
(gl:vertex-attrib-pointer 0 2 :float nil (* 4 2) (cffi:null-pointer))
(gl:enable-vertex-attrib-array 0)
(gl:bind-vertex-array *vertex-array*)

(let* ((time 0)
       (location (gl:get-uniform-location *shader-program* "ourColor")))
  (loop
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (sleep 0.01)
    (incf time)
    (let ((green (+ 0.2 (* 0.8 (/ (+ 1(sin (/ time 50.0))) 2.0)))))
      (gl:uniformf location green))
    (%gl:draw-elements :triangles 6 :unsigned-int 0)
    (sdl2-ffi.functions:sdl-gl-swap-window *window*)))


