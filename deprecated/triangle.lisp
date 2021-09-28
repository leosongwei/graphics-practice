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



(progn
  (defparameter *vertex-shader-string*
    "#version 330 core
layout(location = 0) in vec3 vertexPosition_modelspace;

void main(){
    gl_Position.xyz = vertexPosition_modelspace;
    gl_Position.w = 1.0;
}
")


  (defparameter *fragment-shader-string*
    "#version 330 core
out vec3 color;

void main(){
color = vec3(1,0,0); // red
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
  (defparameter *triangle-points-buffer*
    (float-buffer
     #(-1.0 -1.0  0.0
       1.0 -1.0  0.0
       0.0  1.0  0.0)))

  ;; generate VAO
  (defparameter *vertex-array* (glgen-vertex-array-1))
  (format t "*vertex-array*: ~A~%" *vertex-array*)
  (c-gl-bind-vertex-array *vertex-array*)

  ;; generate VBO
  (defparameter *vertex-buffer* (gl-gen-buffer-1))
  (c-gl-bind-buffer +GL_ARRAY_BUFFER+ *vertex-buffer*)
  ;; send data to VBO
  (c-gl-buffer-data +GL_ARRAY_BUFFER+ (* 4 9)
                    *triangle-points-buffer* +gl_static_draw+)
  ;; set shader vertex attrib pointer
  (c-gl-vertex-attrib-pointer 0 3 +GL_FLOAT+ +GL_FALSE+ 0 null-pointer)
  (c-gl-enable-vertex-attrib-array 0)

  ;;;; draw
  ;; bind VAO
  ;; VAO is like a name space, once bind, will set all parameters,
  ;; such as VBO bindings, vertex attrib pointers...
  (c-gl-bind-vertex-array *vertex-array*)
  ;; draw array
  (c-gl-draw-arrays +gl_triangles+ 0 3)

  (c-sdl-gl-swapwindow *window*))

(c-gl-get-error)
