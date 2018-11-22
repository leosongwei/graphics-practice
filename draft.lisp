(load "math.lisp")
(load "glbind.lisp")

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
    (c-glClearColor 0.0 0.0 0.4 0.0)
    (c-glClear (logior +GL_COLOR_BUFFER_BIT+ +GL_DEPTH_BUFFER_BIT+)))
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

  (defparameter *vertex-array* (glgen-vertex-array-1))
  (c-gl-bind-vertex-array *vertex-array*)

  (progn
    (defparameter *vertex-buffer* (glgenbuffer-1))
    (c-glbindbuffer +GL_ARRAY_BUFFER+ *vertex-buffer*)
    (c-glbufferdata +GL_ARRAY_BUFFER+ (* 4 9) *triangle-points-buffer* +gl_static_draw+))

  (c-glenablevertexattribarray 0)
  (c-glbindbuffer +GL_ARRAY_BUFFER+ *vertex-buffer*)
  (c-glVertexAttribPointer 0 3 +GL_FLOAT+ +GL_FALSE+ 0 null-pointer)
  (c-gl-draw-arrays +gl_triangles+ 0 3)
  (c-gl-disable-vertex-attrib-array 0)

  (c-sdl-gl-swapwindow *window*))

(c-gl-get-error)
