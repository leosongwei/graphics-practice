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

  (c-gl-enable +gl_depth_test+)
  (c-gl-depthfunc +gl_less+)

  (progn
    (c-gl-clear-color 0.2 0.2 0.2 0.0)
    (c-gl-clear (logior +GL_COLOR_BUFFER_BIT+ +GL_DEPTH_BUFFER_BIT+)))
  (c-sdl-gl-swapwindow *window*))


(progn
  (defparameter *vertex-shader-string*
    "#version 330 core
layout(location=0) in vec3 v_pos;
layout(location=1) in vec2 v_uv;
layout(location=2) in vec3 v_normal;

out vec2 tex_coord;
uniform mat4 projection;
uniform mat4 transform;

void main(){
  gl_Position = projection * transform * vec4(v_pos, 1.0f);
  tex_coord = v_uv;
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

  (defparameter *shader-program-id* (create-program-with-shaders *vertex-shader-id*
                                                                 *fragment-shader-id*))
  (c-gl-use-program *shader-program-id*))


(progn
  (defparameter *index-length* 0)
  (defparameter *vao* nil)
  (defparameter *vbo* nil)
  (defparameter *ebo* nil)
  (defparameter *texture-id* nil))

(mvb-let* ((vertices faces (modelmesh-to-array (wavefront-file-to-modelmesh "bunny_high.obj"))))
  ;; (let ((vertex-num (/ (length vertices) 8)))
  ;;   (dotimes (index vertex-num)
  ;;     (format t "~A:" index)
  ;;     (dotimes (i 8)
  ;;       (format t "   ~A" (aref vertices (+ i (* index 8)))))
  ;;     (format t "~%"))
  ;;   (print faces))
  (defparameter *index-length* (length faces))
  (let* ((vertex-buffer (float-buffer vertices))
         (index-buffer (cffi-buffer :uint faces)))
    ;;;; VAO
    (defparameter *vao* (glgen-vertex-array-1))
    (format t "*vertex-array*: ~A~%" *vao*)
    (c-gl-bind-vertex-array *vao*)
    ;;;; VBO
    (defparameter *vbo* (gl-gen-buffer-1))
    (c-gl-bind-buffer +GL_ARRAY_BUFFER+ *vbo*)
    ;; send data to VBO
    (c-gl-buffer-data +GL_ARRAY_BUFFER+ (* (length vertices) (c-sizeof :float))
                      vertex-buffer +gl_static_draw+)
    ;; vertex attrib
    ;; coord, layout location=0
    (c-gl-vertex-attrib-pointer 0 3 +GL_FLOAT+ +GL_FALSE+
                                (* 8 (c-sizeof :float)) null-pointer)
    (c-gl-enable-vertex-attrib-array 0)
    ;; uv, layout location=1
    (c-gl-vertex-attrib-pointer 1 2 +GL_FLOAT+ +GL_FALSE+
                                (* 8 (c-sizeof :float))
                                (cffi:make-pointer (* 3 (c-sizeof :float))))
    (c-gl-enable-vertex-attrib-array 1)
    ;; normal, layout location=2
    (c-gl-vertex-attrib-pointer 2 3 +GL_FLOAT+ +GL_TRUE+
                                (* 8 (c-sizeof :float))
                                (cffi:make-pointer (* 5 (c-sizeof :float))))
    (c-gl-enable-vertex-attrib-array 2)
    ;;;; EBO
    (defparameter *ebo* (gl-gen-buffer-1))
    (c-gl-bind-buffer +GL_ELEMENT_ARRAY_BUFFER+ *ebo*)
    (c-gl-buffer-data +GL_ELEMENT_ARRAY_BUFFER+ (* (length faces) (c-sizeof :float))
                      index-buffer +GL_STATIC_DRAW+)
    ;;;; Texture
    (defparameter *texture-id* (gl-gen-texute-1))
    (c-gl-bind-texture +GL_TEXTURE_2D+ *texture-id*)
    (with-png-buffer image-buffer "./img/wood.png" width height
      (c-gl-tex-image-2d
       +GL_TEXTURE_2D+ 0 +GL_RGB+
       width height 0
       +GL_RGB+ +GL_UNSIGNED_BYTE+
       image-buffer))
    ;; (c-gl-generate-mipmap +GL_TEXTURE_2D+)
    (c-gl-tex-parameter-i +GL_TEXTURE_2D+ +GL_TEXTURE_MAG_FILTER+ +GL_LINEAR+)
    (c-gl-tex-parameter-i +GL_TEXTURE_2D+ +GL_TEXTURE_MIN_FILTER+ +GL_LINEAR+)))

;;;; draw
(progn
  (c-gl-clear (logior +GL_COLOR_BUFFER_BIT+ +GL_DEPTH_BUFFER_BIT+))
  (c-gl-bind-vertex-array *vao*)
  (c-gl-use-program *shader-program-id*)
  (c-gl-uniform-1i (gl-get-uniform-location *shader-program-id* "tex") 0)
  (c-gl-active-texture +GL_TEXTURE0+)
  (c-gl-bind-texture +GL_TEXTURE_2D+ *texture-id*)

  ;; transform
  (let* ((project-mat (frustum-mat 10 (/ 4 3) 0.1 15))
         (trans-mat (3d-trans-mat 0.05 -0.35 -10.0))
         (scale-mat (3d-scale 5.0))
         (trans-world (mul-44-44 trans-mat scale-mat))
         (project-mat-buffer (alloc-mat44f))
         (trans-world-buffer (alloc-mat44f)))
    (update-mat44f-buffer project-mat project-mat-buffer)
    (update-mat44f-buffer trans-world trans-world-buffer)
    (c-gl-uniform-matrix-4fv (gl-get-uniform-location *shader-program-id* "projection")
                             1 +GL_TRUE+ project-mat-buffer)
    (c-gl-uniform-matrix-4fv (gl-get-uniform-location *shader-program-id* "transform")
                             1 +GL_TRUE+ trans-world-buffer)
    (c-gl-draw-elements +GL_TRIANGLES+ *index-length* +GL_UNSIGNED_INT+ null-pointer)
    (let* ((trans-mat (3d-trans-mat 0.25 0.0 -13.0))
           (rot-mat (3d-rotate-y 90))
           (trans-world (mul-44-44 trans-mat (mul-44-44 rot-mat scale-mat))))
      (update-mat44f-buffer trans-world trans-world-buffer)
      (c-gl-uniform-matrix-4fv (gl-get-uniform-location *shader-program-id* "transform")
                               1 +GL_TRUE+ trans-world-buffer)
      (c-gl-draw-elements +GL_TRIANGLES+ *index-length* +GL_UNSIGNED_INT+ null-pointer))))

(c-sdl-gl-swapwindow *window*)
