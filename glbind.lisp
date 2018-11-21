(ql:quickload 'cffi)

(cffi:use-foreign-library "libGLEW.so")
(cffi:use-foreign-library "libGLU.so")
(cffi:use-foreign-library "libGL.so")
(cffi:use-foreign-library "libGLX.so")
(cffi:use-foreign-library "libSDL2.so")

;; GLenum glewErr = glewInit();
(cffi:defcfun (c-glewinit "glewInit") :uint)

(defun float-buffer (float-array)
  (cffi:foreign-alloc :float :initial-contents float-array))
;;(cffi:mem-aref (float-buffer #(1.0 2.0 3.0 4.0)) :float 1) => 2.0

(defparameter null-pointer (cffi:null-pointer))

(defmacro defenum (&body list)
  (append
   '(progn)
   (let ((params '()))
     (dotimes (i (length list))
       (push `(defparameter ,(nth i list) ,i) params))
     params)))

(defenum
  +SDL_GL_RED_SIZE+
  +SDL_GL_GREEN_SIZE+
  +SDL_GL_BLUE_SIZE+
  +SDL_GL_ALPHA_SIZE+
  +SDL_GL_BUFFER_SIZE+
  +SDL_GL_DOUBLEBUFFER+
  +SDL_GL_DEPTH_SIZE+
  +SDL_GL_STENCIL_SIZE+
  +SDL_GL_ACCUM_RED_SIZE+
  +SDL_GL_ACCUM_GREEN_SIZE+
  +SDL_GL_ACCUM_BLUE_SIZE+
  +SDL_GL_ACCUM_ALPHA_SIZE+
  +SDL_GL_STEREO+
  +SDL_GL_MULTISAMPLEBUFFERS+
  +SDL_GL_MULTISAMPLESAMPLES+
  +SDL_GL_ACCELERATED_VISUAL+
  +SDL_GL_RETAINED_BACKING+
  +SDL_GL_CONTEXT_MAJOR_VERSION+
  +SDL_GL_CONTEXT_MINOR_VERSION+
  +SDL_GL_CONTEXT_EGL+
  +SDL_GL_CONTEXT_FLAGS+
  +SDL_GL_CONTEXT_PROFILE_MASK+
  +SDL_GL_SHARE_WITH_CURRENT_CONTEXT+
  +SDL_GL_FRAMEBUFFER_SRGB_CAPABLE+
  +SDL_GL_CONTEXT_RELEASE_BEHAVIOR+
  +SDL_GL_CONTEXT_RESET_NOTIFICATION+
  +SDL_GL_CONTEXT_NO_ERROR+)
(defparameter +SDL_INIT_VIDEO+ #x00000020)

(defparameter +SDL_GL_CONTEXT_PROFILE_CORE+ #x0001)
(defparameter +SDL_GL_CONTEXT_PROFILE_COMPATIBILITY+ #x0002)
(defparameter +SDL_GL_CONTEXT_PROFILE_ES+ #x0004)

(defparameter +SDL_WINDOW_FULLSCREEN+ #x00000001)
(defparameter +SDL_WINDOW_OPENGL+ #x00000002)
(defparameter +SDL_WINDOW_SHOWN+ #x00000004)
(defparameter +SDL_WINDOW_HIDDEN+ #x00000008)
(defparameter +SDL_WINDOW_BORDERLESS+ #x00000010)
(defparameter +SDL_WINDOW_RESIZABLE+ #x00000020)
(defparameter +SDL_WINDOW_MINIMIZED+ #x00000040)
(defparameter +SDL_WINDOW_MAXIMIZED+ #x00000080)
(defparameter +SDL_WINDOW_INPUT_GRABBED+ #x00000100)
(defparameter +SDL_WINDOW_INPUT_FOCUS+ #x00000200)
(defparameter +SDL_WINDOW_MOUSE_FOCUS+ #x00000400)
(defparameter +SDL_WINDOW_FULLSCREEN_DESKTOP+ (logior +SDL_WINDOW_FULLSCREEN+ #x00001000))
(defparameter +SDL_WINDOW_FOREIGN+ #x00000800)
(defparameter +SDL_WINDOW_ALLOW_HIGHDPI+ #x00002000)

;;#define SDL_WINDOWPOS_UNDEFINED_MASK    0x1FFF0000u
;;#define SDL_WINDOWPOS_UNDEFINED_DISPLAY(X)  (SDL_WINDOWPOS_UNDEFINED_MASK|(X))
;;#define SDL_WINDOWPOS_UNDEFINED         SDL_WINDOWPOS_UNDEFINED_DISPLAY(0)
(defparameter +SDL_WINDOWPOS_UNDEFINED_MASK+ #x1FFF0000)
(defparameter +SDL_WINDOWPOS_UNDEFINED+ +SDL_WINDOWPOS_UNDEFINED_MASK+)

;; int SDL_Init(Uint32 flags)
(cffi:defcfun (c-sdl-init "SDL_Init") :int (flags :uint))

;; int SDL_GL_SetAttribute(SDL_GLattr attr, int value)
(cffi:defcfun (c-sdl-gl-setattribute "SDL_GL_SetAttribute") :int
  (attr :uchar) (value :int))

(defun init-sdl-gl-attribute ()
  (c-sdl-gl-setattribute +sdl_gl_context_major_version+ 3)
  (c-sdl-gl-setattribute +sdl_gl_context_minor_version+ 3)
  (c-sdl-gl-setattribute +SDL_GL_CONTEXT_PROFILE_MASK+ +SDL_GL_CONTEXT_PROFILE_CORE+))

;; SDL_Window* SDL_CreateWindow(const char* title, int x, int y, int w, int h, Uint32 flags)
(cffi:defcfun (c-sdl-createwindow "SDL_CreateWindow") :pointer
  (title :pointer) (x :int) (y :int) (w :int) (h :int) (flags :uint))

;; SDL_GLContext SDL_GL_CreateContext(SDL_Window* window)
;; typedef void *SDL_GLContext;
(cffi:defcfun (c-sdl-gl-createcontext "SDL_GL_CreateContext") :pointer
  (window :pointer))

;; void SDL_GL_SwapWindow(SDL_Window* window)
(cffi:defcfun (c-sdl-gl-swapwindow "SDL_GL_SwapWindow") :void
  (window :pointer))

;; ---------OpenGL------------
(defparameter +GL_FLOAT+ #x1406)
(defparameter +GL-TRUE+ 1)
(defparameter +GL-FALSE+ 0)
(defparameter +GL_STATIC_DRAW+ #x88E4)
(defparameter +GL_ARRAY_BUFFER+ #x8892)
(defparameter +GL_COLOR_BUFFER_BIT+ #x00004000)
(defparameter +GL_DEPTH_BUFFER_BIT+ #x00000100)
(defparameter +GL_LESS+ #x0201)
(defparameter +GL_DEPTH_TEST+ #x0B71)

;; typedef int GLsizei;
;; typedef unsigned int GLuint;
;; void glGenBuffers(GLsizei n, GLuint * buffers);
(cffi:defcfun (c-glGenBuffers "glGenBuffers") :void
  (n :int) (buffers :pointer))
(defun glgenbuffer-1 ()
  (cffi:with-foreign-object (new-buffer :int)
    (c-glgenbuffers 1 new-buffer)
    (cffi:mem-ref new-buffer :int)))

;; typedef unsigned int GLenum;
;; void glBindBuffer(GLenum target, GLuint buffer);
(cffi:defcfun (c-glBindBuffer "glBindBuffer") :void
  (target :uint) (buffer :int))

;; void glBufferData(GLenum target, GLsizeiptr size, const GLvoid * data, GLenum usage);
;; glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data),
;;              g_vertex_buffer_data, GL_STATIC_DRAW);
(cffi:defcfun (c-glBufferData "glBufferData") :void
  (target :uint) (size :long) (data :pointer) (usage :uint))

;; void glEnableVertexAttribArray(GLuint index);
(cffi:defcfun (c-glEnableVertexAttribArray "glEnableVertexAttribArray") :void
  (index :uint))

;; typedef unsigned char GLboolean;
;; void glVertexAttribPointer(GLuint index,
;;  	GLint size,
;;  	GLenum type,
;;  	GLboolean normalized,
;;  	GLsizei stride,
;; 	const GLvoid * pointer);
(cffi:defcfun (c-glVertexAttribPointer "glVertexAttribPointer") :void
  (index :uint) (size :int) (type :uint)
  (normalized :uchar) (stride :long) (pointer :pointer))

;; void glClear(GLbitfield mask);
(cffi:defcfun (c-glClear "glClear") :void
  (mask :ulong))

;; void glClearColor(GLclampf red,  GLclampf green,  GLclampf blue,  GLclampf alpha);
;; typedef float GLclampf;
(cffi:defcfun (c-glClearColor "glClearColor") :void
  (red :float) (green :float) (blue :float) (alpha :float))

;; void glEnable(GLenum cap);
(cffi:defcfun (c-gl-enable "glEnable") :void (cap :uint))

;; void glDepthFunc(GLenum func);
(cffi:defcfun (c-gl-depthfunc "glDepthFunc") :void (func :uint))
