(load "math.lisp")
(load "glbind.lisp")

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
  (c-glClearColor 0.0 0.0 0.4 0.0)
  (c-glClear (logior +GL_COLOR_BUFFER_BIT+ +GL_DEPTH_BUFFER_BIT+)))
(c-sdl-gl-swapwindow *window*)


(defparameter *triangle-points-buffer*
  (float-buffer
   #(-1.0 -1.0 0.0
     1.0  -1.0 0.0
     0.0  1.0  0.0)))

(progn
  (defparameter *vertex-buffer* (glgenbuffer-1))
  (c-glbindbuffer +GL_ARRAY_BUFFER+ *vertex-buffer*)
  (c-glbufferdata +GL_ARRAY_BUFFER+ (* 4 9) *triangle-points-buffer* +gl_static_draw+))

(c-glenablevertexattribarray 0)
(c-glbindbuffer +GL_ARRAY_BUFFER+ *vertex-buffer*)
(c-glVertexAttribPointer 0 3 +GL_FLOAT+ +GL-FALSE+ 0 null-pointer)

(c-sdl-gl-swapwindow *window*)
