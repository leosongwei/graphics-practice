(load "../load-libs")
(ql:quickload 'cxx-jit)
(use-package :3d-vectors)
(use-package :3d-matrices)

(load "opencv")

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
    (sdl2-ffi.functions:sdl-gl-create-context *window*))
  (%gl:viewport 0 0 640 480)
  (gl:clear-color 0.2 0.2 0.2 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (sdl2-ffi.functions:sdl-gl-swap-window *window*))

(progn
  (defparameter *vertex-shader*
    "
#version 330 core
layout(location = 0) in vec3 model_coord;
layout(location = 1) in vec3 model_normal;
layout(location = 2) in vec2 tex_coord;
uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
out vec2 frag_tex_coord;

void main(){
    frag_tex_coord = tex_coord;
    gl_Position = projection * view * model * vec4(model_coord, 1.0);
}
")

  (defparameter *fragment-shader*
    "
#version 330 core

uniform sampler2D box_texture;
in vec2 frag_tex_coord;
out vec3 color;

void main(){
    color = vec3(texture(box_texture, frag_tex_coord));
}
")

  (let* ((vs (compile-shader-from-string :vertex-shader *vertex-shader*))
         (fs (compile-shader-from-string :fragment-shader *fragment-shader*)))
    (defparameter *shader-program* (create-program-with-shaders vs fs))
    (gl:delete-shader vs)
    (gl:delete-shader fs)))



(defparameter *camera* (cv-video-capture))
(with-camera-read (frame *camera*)
  (cv-cvt-color-bgr-rgba frame)
  (multiple-value-bind (width height) (cv-mat-shape frame)
    (defparameter *camera-view-texture*
      (make-2d-texture-from-buffer (cv-mat-data frame) width height))))
(defparameter *camera-view-vao* (make-unit-square-vao))


(progn
  (gl:enable :depth-test)
  (gl:bind-vertex-array *camera-view-vao*)
  (gl:use-program *shader-program*)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d *camera-view-texture*)

  (let ((proj-loc (gl:get-uniform-location *shader-program* "projection"))
      (view-loc (gl:get-uniform-location *shader-program* "view"))
      (model-loc (gl:get-uniform-location *shader-program* "model"))
      (box-texture-location (gl:get-uniform-location *shader-program* "box_texture")))
  (defun set-shader-uniforms (proj-mat view-mat model-mat)
    (%gl:uniform-1i box-texture-location 0)
    (3d-cffi:with-c-mat (c-proj proj-mat)
      (%gl:uniform-matrix-4fv proj-loc 1 t c-proj))
    (3d-cffi:with-c-mat (c-view view-mat)
      (%gl:uniform-matrix-4fv view-loc 1 t c-view))
    (3d-cffi:with-c-mat (c-model model-mat)
      (%gl:uniform-matrix-4fv model-loc 1 t c-model)))))

(loop
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (set-shader-uniforms (meye 4)
                       (meye 4)
                       (meye 4))
  (with-camera-read (frame *camera*)
    (cv-cvt-color-bgr-rgba frame)
    (cv-flip frame 0)
    (multiple-value-bind (width height) (cv-mat-shape frame)
      (bind-update-2d-texture-from-buffer
       *camera-view-texture*
       (cv-mat-data frame)
       width height)))
  (%gl:draw-elements :triangles 6 :unsigned-int 0)
  (sdl2-ffi.functions:sdl-gl-swap-window *window*))
