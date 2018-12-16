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
    (c-gl-clear-color 0.2 0.2 0.2 0.0)
    (c-gl-clear (logior +GL_COLOR_BUFFER_BIT+ +GL_DEPTH_BUFFER_BIT+)))
  (c-sdl-gl-swapwindow *window*))

(progn
  (defparameter *vertex-shader-string*
    "#version 330 core
layout(location=0) in vec3 vpos;

uniform mat4 trans_mat;

void main(){
  gl_Position = project * view * model * vec4(vpos, 1.0f);
}
")


  (defparameter *fragment-shader-string*
    "#version 330 core
out vec3 color;

void main(){
  color = vec3(0.8f, 0.1f, 0.1f);
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



