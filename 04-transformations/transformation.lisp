(load "../load-libs")

(use-package :3d-vectors)
(use-package :3d-matrices)

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
       1920 1080
       (logior sdl2-ffi:+sdl-window-opengl+ sdl2-ffi:+sdl-window-shown+))))

  (defparameter *glcontext*
    (sdl2-ffi.functions:sdl-gl-create-context *window*))
  (%gl:viewport 0 0 1920 1080)
  (gl:clear-color 0.2 0.2 0.2 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (sdl2-ffi.functions:sdl-gl-swap-window *window*))

(progn
  (defparameter *vertex-shader*
    "
#version 330 core
layout(location = 0) in vec3 model_coord;
layout(location = 1) in vec2 tex_coord;
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

(load "../resources/box.lisp")
;;;; defines *box-vertices*, format: x y z u v, length: 36 vertices
(defparameter *box-indices*
  (make-array 36 :element-type 'unsigned-int
                 :initial-contents (loop for i from 0 to 35 collect i)))
(defparameter *box-vao* (make-vao *box-vertices* *box-indices*
                                  '((3 :float) (2 :float))))

(defparameter *box-texture* (make-2d-texture "../resources/wood_square.png"))

(progn
  (gl:enable :depth-test)
  (gl:bind-vertex-array *box-vao*)
  (gl:use-program *shader-program*)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d *box-texture*))

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
      (%gl:uniform-matrix-4fv model-loc 1 t c-model))))

(defparameter *cube-positions*
  (loop repeat 100
        collect (vec (- (random 30) 15)
                     (- (random 10) 5)
                     (- (random 30) 15))))

(loop
(dotimes (frame-index 300)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (let* ((progress (/ frame-index 300))
         (angle (* 2 pi progress))
         (eye (vec (* 20 (sin angle))
                   3
                   (* 20 (cos angle))))
         (proj (mperspective 45 (/ 16 9) 0.1 100))
         ;;(proj (mortho -5 5 -5 5 10 40))
         (up (vunit (vc (vrot +vx+ +vy+ angle) (v- (vec 0 0 0) eye))))
         (view (mlookat eye (vec 0 0 0) up)))
    (let ((i 0))
      (dolist (position *cube-positions*)
        (incf i)
        (let* ((translation (mtranslation position))
               (rotated (nmrotate translation (vec 1 0.3 0.5)
                                  (+ (* 20 i)
                                     (* 2 pi progress)))))
          (set-shader-uniforms proj view rotated)
          (%gl:draw-elements :triangles 36 :unsigned-int 0))))
    (sdl2-ffi.functions:sdl-gl-swap-window *window*))
  )
)
