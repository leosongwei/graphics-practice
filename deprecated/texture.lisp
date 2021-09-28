(load "load-modules.lisp")

(progn
  (c-sdl-init +sdl_init_video+)
  (c-sdl-gl-setattribute +sdl_gl_context_major_version+ 3)
  (c-sdl-gl-setattribute +sdl_gl_context_major_version+ 3)
  (c-sdl-gl-setattribute +sdl_gl_context_profile_mask+ +sdl_gl_context_profile_core+)

  (defparameter *window*
    (cffi:with-foreign-string (title "test")
      (c-sdl-createwindow title +sdl_windowpos_undefined+ +sdl_windowpos_undefined+
                          640 480
                          (logior +sdl_window_opengl+ +sdl_window_shown+))))

  (defparameter *glcontext*
    (c-sdl-gl-createcontext *window*))

  (c-glewinit)
  ;;(c-gl-enable +gl_depth_test+)
  ;;(c-gl-depthfunc +gl_less+)

  (progn
    (c-gl-clear-color 0.2 0.2 0.2 0.0)
    (c-gl-clear (logior +GL_COLOR_BUFFER_BIT+ +GL_DEPTH_BUFFER_BIT+)))
  (c-sdl-gl-swapwindow *window*))

;;;; shader
(progn
  (defparameter *vertex-shader-string*
    "#version 330 core
layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec2 vtexCoord;

out vec2 tex_coord;

void main(){
    tex_coord = vtexCoord;
    gl_Position = vec4(vertexPosition_modelspace, 1.0);
}
")

  (defparameter *fragment-shader-string*
    "#version 330 core
in vec2 tex_coord;
out vec3 color;
uniform sampler2D tex;

void main(){
    color = vec3(texture(tex, tex_coord));
}
")

  (defparameter *vertex-shader-id*
    (compile-shader-from-string +GL_VERTEX_SHADER+ *vertex-shader-string*))
  (format t "vs id:~A~%" *vertex-shader-id*)
  (defparameter *fragment-shader-id*
    (compile-shader-from-string +GL_FRAGMENT_SHADER+ *fragment-shader-string*))
  (format t "fs id:~A~%" *fragment-shader-id*)
  (defparameter *shader* (create-program-with-shaders *vertex-shader-id*
                                                      *fragment-shader-id*)))

(progn

  (defparameter *triangle-vertex-buffer*
    (float-buffer
     #(0.0  0.5  0.0 #||# 0.5 1.0
       0.5 -0.5  0.0 #||# 1.0 0.0
      -0.5 -0.5  0.0 #||# 0.0 0.0)))
  (defparameter *triangle-indices*
    (cffi-buffer
     :uint
     #(0 1 2)))
  ;; VAO
  (defparameter *vao* (glgen-vertex-array-1))
  (format t "*vertex-array*: ~A~%" *vao*)
  (c-gl-bind-vertex-array *vao*)
  ;; generate VBO
  (defparameter *vbo* (gl-gen-buffer-1))
  (c-gl-bind-buffer +GL_ARRAY_BUFFER+ *vbo*)
  ;; send data to VBO
  (c-gl-buffer-data +GL_ARRAY_BUFFER+ (* (* 5 3) (c-sizeof :float))
                    *triangle-vertex-buffer* +gl_static_draw+)
  ;;; vertex attrib
  ;; coord, layout location=0
  (c-gl-vertex-attrib-pointer 0 3 +GL_FLOAT+ +GL_FALSE+
                              (* 5 (c-sizeof :float)) null-pointer)
  (c-gl-enable-vertex-attrib-array 0)
  ;; color, layout location=1
  (c-gl-vertex-attrib-pointer 1 2 +GL_FLOAT+ +GL_FALSE+
                              (* 5 (c-sizeof :float))
                              (cffi:make-pointer (* 3 (c-sizeof :float))))
  (c-gl-enable-vertex-attrib-array 1)
  ;; EBO
  (defparameter *ebo* (gl-gen-buffer-1))
  (c-gl-bind-buffer +GL_ELEMENT_ARRAY_BUFFER+ *ebo*)
  (c-gl-buffer-data +GL_ELEMENT_ARRAY_BUFFER+ (* 3 (c-sizeof :float))
                    *triangle-indices* +GL_STATIC_DRAW+)
  ;; Texture
  (defparameter *texture-id* (gl-gen-texute-1))
  (c-gl-bind-texture +GL_TEXTURE_2D+ *texture-id*)
  (with-sdl-image (image-buffer "./img/th06.png" width height)
    (c-gl-tex-image-2d
     +GL_TEXTURE_2D+ 0 +GL_RGB+
     width height 0
     +GL_RGB+ +GL_UNSIGNED_BYTE+
     image-buffer))
  (c-gl-tex-parameter-i +GL_TEXTURE_2D+ +GL_TEXTURE_MAG_FILTER+ +GL_NEAREST+)
  (c-gl-tex-parameter-i +GL_TEXTURE_2D+ +GL_TEXTURE_MIN_FILTER+ +GL_NEAREST+)
  ;; (cffi:with-foreign-object (img-buffer :uint8 (* 32 32 3))
  ;;   (dotimes (i (* 32 32 3))
  ;;     (setf (cffi:mem-aref img-buffer :uint8) 255))
  ;;   (c-gl-tex-image-2d
  ;;    +GL_TEXTURE_2D+ 0 +GL_RGB+
  ;;    32 32 0
  ;;    +GL_RGB+ +GL_UNSIGNED_BYTE+
  ;;    img-buffer))



  ;;;; draw
  ;; draw triangle
  (c-gl-bind-vertex-array *vao*)

  (c-gl-use-program *shader*)
  (c-gl-uniform-1i (gl-get-uniform-location *shader* "tex") 0)

  (c-gl-active-texture +GL_TEXTURE0+)
  (c-gl-bind-texture +GL_TEXTURE_2D+ *texture-id*)

  (c-gl-draw-elements +GL_TRIANGLES+ 3 +GL_UNSIGNED_INT+ null-pointer)

  (c-sdl-gl-swapwindow *window*))

(c-gl-get-error)
