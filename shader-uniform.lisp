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
  (defparameter *quad-vertex-shader*
    "
#version 330 core
layout(location = 0) in vec2 vertexPosition_modelspace;
void main(){
    gl_Position.xy = vertexPosition_modelspace;
    gl_Position.z = 0.0;
    gl_Position.w = 1.0;
}
")

  (defparameter *quad-fragment-shader*
    "
#version 330 core
out vec3 color;

uniform float ourColor;

void main(){
    color.g = ourColor;
    color.r = 0.2;
    color.b = 0.2;
}
")

  (let* ((vs (compile-shader-from-string :vertex-shader *quad-vertex-shader*))
         (fs (compile-shader-from-string :fragment-shader *quad-fragment-shader*)))
    (defparameter *quad-shader-program* (create-program-with-shaders vs fs))
    (gl:delete-shader vs)
    (gl:delete-shader fs)))

(progn
  (defparameter *triangle-vertex-shader*
    "
#version 330 core
layout(location = 0) in vec2 vertexPosition_modelspace;
layout(location = 1) in vec3 vertexColor;

out vec3 vColor;

void main(){
    gl_Position.xy = vertexPosition_modelspace;
    gl_Position.z = 0.0;
    gl_Position.w = 1.0;
    vColor = vertexColor;
}
")

  (defparameter *triangle-fragment-shader*
    "
#version 330 core
in vec3 vColor;
out vec3 color;

uniform float ourColor;

void main(){
    color = vColor;
}
")

  (let* ((vs (compile-shader-from-string :vertex-shader *triangle-vertex-shader*))
         (fs (compile-shader-from-string :fragment-shader *triangle-fragment-shader*)))
    (defparameter *triangle-shader-program* (create-program-with-shaders vs fs))
    (gl:delete-shader vs)
    (gl:delete-shader fs)))

(defun make-vao (vertex-array indicies-array attrib-lengths-and-gl-types)
  (let ((vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer))
        (ebo (gl:gen-buffer)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (with-c-buffer (buffer vertex-array :float)
      (%gl:buffer-data :array-buffer (* (length vertex-array) (cffi:foreign-type-size :float))
                       buffer :static-draw))
    (gl:bind-buffer :element-array-buffer ebo)
    (with-c-buffer (buffer indicies-array :uint32)
      (%gl:buffer-data :element-array-buffer (* (length indicies-array) (cffi:foreign-type-size :uint32))
                       buffer :static-draw))
    (let ((attrib-index 0)
          (offset 0)
          (attrib-total-size
            (let ((size 0))
              (dolist (attrib-length-and-gl-type attrib-lengths-and-gl-types size)
                (destructuring-bind (attrib-length gl-type) attrib-length-and-gl-type
                  (setf size (+ size
                                (* attrib-length (cffi:foreign-type-size
                                                  (gl-type-to-cffi-type gl-type))))))))))
      (mapcar (lambda (attrib-length-and-gl-type)
                (destructuring-bind (attrib-length gl-type) attrib-length-and-gl-type
                  (let* ((cffi-type (gl-type-to-cffi-type gl-type))
                         (attrib-size (* attrib-length (cffi:foreign-type-size cffi-type))))
                    (gl:vertex-attrib-pointer attrib-index attrib-length
                                              gl-type nil attrib-total-size
                                              (cffi:make-pointer offset))
                    (setf offset (+ offset attrib-size))
                    (gl:enable-vertex-attrib-array attrib-index)
                    (incf attrib-index))))
              attrib-lengths-and-gl-types))
    vao))

(defparameter *quad-points*
  #(-0.9 0.5
    -0.1 0.5
    -0.9 -0.5
    -0.1 -0.5))
(defparameter *quad-indicies* #(0 3 1 0 2 3))
(defparameter *quad-vao* (make-vao *quad-points* *quad-indicies* '((2 :float))))

(defparameter *triangle*
  ;; vec2: coord, vec3 color
  #(0.5 0.5  1.0 0.1 0.1
    0.1 -0.5 0.1 1.0 0.1
    0.9 -0.5 0.1 0.1 1.0))
(defparameter *triangle-indicies* #(0 1 2))
(defparameter *triangle-vao*
  (make-vao *triangle* *triangle-indicies* '((2 :float) (3 :float))))

(let* ((time 0)
       (location (gl:get-uniform-location *quad-shader-program* "ourColor")))
  (loop
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (sleep 0.01)

    (incf time)
    (gl:bind-vertex-array *quad-vao*)
    (gl:use-program *quad-shader-program*)
    (let ((green (+ 0.2 (* 0.8 (/ (+ 1 (sin (/ time 25.0))) 2.0)))))
      (gl:uniformf location green))
    (%gl:draw-elements :triangles 6 :unsigned-int 0)

    (gl:bind-vertex-array *triangle-vao*)
    (gl:use-program *triangle-shader-program*)
    (%gl:draw-elements :triangles 3 :unsigned-int 0)

    (sdl2-ffi.functions:sdl-gl-swap-window *window*)))


