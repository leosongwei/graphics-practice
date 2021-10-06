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
  (defparameter *quad-vertex-shader*
    "
#version 330 core
layout(location = 0) in vec2 modelCoord;
layout(location = 1) in vec2 texCoord;

out vec2 fragTexCoord;

void main(){
    fragTexCoord = texCoord;
    gl_Position.xy = modelCoord;
    gl_Position.z = 0.0;
    gl_Position.w = 1.0;
}
")

  (defparameter *quad-fragment-shader*
    "
#version 330 core

uniform sampler2D quadTexture;
in vec2 fragTexCoord;
out vec3 color;

void main(){
    color = vec3(texture(quadTexture, fragTexCoord));
}
")

  (let* ((vs (compile-shader-from-string :vertex-shader *quad-vertex-shader*))
         (fs (compile-shader-from-string :fragment-shader *quad-fragment-shader*)))
    (defparameter *quad-shader-program* (create-program-with-shaders vs fs))
    (gl:delete-shader vs)
    (gl:delete-shader fs)))

(defparameter *quad-points*
  ;; x    y     u    v
  #(-0.9  0.9   0.0  1.0
     0.9  0.9   1.0  1.0
    -0.9 -0.9   0.0  0.0
     0.9 -0.9   1.0  0.0))
(defparameter *quad-indicies* #(0 3 1 0 2 3))
(defparameter *quad-vao* (make-vao *quad-points* *quad-indicies* '((2 :float) (2 :float))))

(defparameter *gl-texture* (gl:gen-texture))
(gl:bind-texture :texture-2d *gl-texture*)
(gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
(gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
(gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
(gl:tex-parameter :texture-2d :texture-mag-filter :linear)
(stb:stbi-set-flip-vertically-on-load t)
(stb:with-stbi-load (image-pointer w h channels) "../resources/th06.png"
  channels ;; ignore
  (gl:tex-image-2d :texture-2d 0 :rgb w h 0 :rgb :unsigned-byte image-pointer))
(gl:generate-mipmap :texture-2d)

(gl:bind-vertex-array *quad-vao*)
(gl:use-program *quad-shader-program*)
(%gl:uniform-1i (gl:get-uniform-location *quad-shader-program* "quadTexture")
                13)
(gl:active-texture :texture13)
(gl:bind-texture :texture-2d *gl-texture*)
(%gl:draw-elements :triangles 6 :unsigned-int 0)

(sdl2-ffi.functions:sdl-gl-swap-window *window*)
