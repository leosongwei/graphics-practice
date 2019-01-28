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
  (c-gl-enable +gl_cull_face+)

  (progn
    (c-gl-clear-color 0.0 0.0 0.0 0.0)
    (c-gl-clear (logior +GL_COLOR_BUFFER_BIT+ +GL_DEPTH_BUFFER_BIT+)))
  (c-sdl-gl-swapwindow *window*))


(progn
  (defparameter *vertex-shader-string*
    "#version 330 core
layout(location=0) in vec3 v_pos;
layout(location=1) in vec2 v_uv;
layout(location=2) in vec3 v_normal;

out vec2 tex_coord;
out vec3 normal;
out vec3 frag_pos;

uniform mat4 projection;
uniform mat4 trans_view;
uniform mat4 trans_model;

void main(){
  gl_Position = projection * trans_view * trans_model * vec4(v_pos, 1.0f);
  tex_coord = v_uv;
  // normal = normalize(vec3(trans_model * vec4(v_normal, 1.0f)));
  normal = normalize(mat3(transpose(inverse(trans_model))) * v_normal);
  frag_pos = vec3(trans_model * vec4(v_pos, 1.0f));
}
")


  (defparameter *fragment-shader-string* (read-file-as-string "gold.glsl"))

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
  (defparameter *texture-id* nil)
  (defparameter *cube-map-id* nil))

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
    (c-gl-tex-parameter-i +GL_TEXTURE_2D+ +GL_TEXTURE_MIN_FILTER+ +GL_LINEAR+)

    (defparameter *cube-map-id* (gl-gen-texute-1))
    (c-gl-bind-texture +GL_TEXTURE_CUBE_MAP+ *cube-map-id*)
    (let* ((files '("posx" "negx" "posy" "negy" "posz" "negz"))
           ;; ./img/skybox/posx.png
           (build-path (lambda (file) (concatenate 'string "./img/skybox/" file ".png")))
           (path-list (mapcar build-path files))
           (gl-target-num +GL_TEXTURE_CUBE_MAP_POSITIVE_X+))
      (dolist (path path-list)
        (with-png-buffer image-buffer path width height
          (c-gl-tex-image-2d
           gl-target-num 0 +GL_RGB+
           width height 0
           +GL_RGB+ +GL_UNSIGNED_BYTE+
           image-buffer))
        (incf gl-target-num)))
    (c-gl-tex-parameter-i +GL_TEXTURE_CUBE_MAP+ +GL_TEXTURE_MIN_FILTER+ +GL_LINEAR+)
    (c-gl-tex-parameter-i +GL_TEXTURE_CUBE_MAP+ +GL_TEXTURE_MAG_FILTER+ +GL_LINEAR+)
    (c-gl-tex-parameter-i +GL_TEXTURE_CUBE_MAP+ +GL_TEXTURE_WRAP_S+ +GL_CLAMP_TO_EDGE+)
    (c-gl-tex-parameter-i +GL_TEXTURE_CUBE_MAP+ +GL_TEXTURE_WRAP_T+ +GL_CLAMP_TO_EDGE+)
    (c-gl-tex-parameter-i +GL_TEXTURE_CUBE_MAP+ +GL_TEXTURE_WRAP_R+ +GL_CLAMP_TO_EDGE+)
    ))

;;;; draw
(progn
  (c-gl-clear (logior +GL_COLOR_BUFFER_BIT+ +GL_DEPTH_BUFFER_BIT+))
  (c-gl-bind-vertex-array *vao*)
  (c-gl-use-program *shader-program-id*)
  (c-gl-uniform-1i (gl-get-uniform-location *shader-program-id* "tex") 0)
  (c-gl-uniform-1i (gl-get-uniform-location *shader-program-id* "skybox") 1)

  (c-gl-active-texture +GL_TEXTURE0+)
  (c-gl-bind-texture +GL_TEXTURE_2D+ *texture-id*)
  (c-gl-active-texture (1+ +GL_TEXTURE0+))
  (c-gl-bind-texture +GL_TEXTURE_CUBE_MAP+ *cube-map-id*)

  ;; transform
  (loop
     (dotimes (i 360)
       (sleep (/ 1 60))
       (c-gl-clear (logior +GL_COLOR_BUFFER_BIT+ +GL_DEPTH_BUFFER_BIT+))
       (let* ((project-mat (frustum-mat 7.5 (/ 4 3) 0.1 15))
              (trans-view (3d-trans-mat 0.05 -0.35 -10.0))
              ;; --
              (rot-mat (3d-rotate-y i))
              (scale-mat (3d-scale 5.0))
              (trans-model (mul-44-44 rot-mat scale-mat)))
         (gl-uniform-mat4fv "projection" project-mat)
         (gl-uniform-mat4fv "trans_view" trans-view)
         (gl-uniform-mat4fv "trans_model" trans-model)
         (c-gl-draw-elements +GL_TRIANGLES+ *index-length* +GL_UNSIGNED_INT+ null-pointer)
         ;; (let* ((trans-mat (3d-trans-mat 0.25 0.0 -13.0))
         ;;        (rot-mat (3d-rotate-y 90))
         ;;        (trans-world (mul-44-44 trans-mat (mul-44-44 rot-mat scale-mat))))
         (let* ((trans (3d-trans-mat 0.2 0.35 -3.0))
                (rot-mat (3d-rotate-y 90))
                (trans-model (mul-44-44 trans (mul-44-44 rot-mat scale-mat))))
           (gl-uniform-mat4fv "trans_model" trans-model)
           (c-gl-draw-elements +GL_TRIANGLES+ *index-length* +GL_UNSIGNED_INT+ null-pointer)))
       (c-sdl-gl-swapwindow *window*))))
