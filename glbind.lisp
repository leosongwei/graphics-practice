(ql:quickload 'cffi)

(cffi:use-foreign-library "libGLEW.so")
(cffi:use-foreign-library "libglfw.so")

(defun float-buffer (float-array)
  (cffi:foreign-alloc :float :initial-contents float-array))
;;(cffi:mem-aref (float-buffer #(1.0 2.0 3.0 4.0)) :float 1) => 2.0

(defparameter *error-message* nil)
(defparameter *error-code* nil)
;;void ErrorCallback(int, const char* err_str)
(cffi:defcallback glew-error-callback :void ((error-code :int) (string :pointer))
  (let ((message (cffi:foreign-string-to-lisp string)))
    (setf *error-message* message)
    (setf *error-code* (cffi:mem-ref error-code :int))))

(cffi:defcfun (c-glfwSetErrorCallback "glfwSetErrorCallback") :pointer
  (function :pointer))

(c-glfwseterrorcallback (cffi:callback glew-error-callback))



;; int glfwInit(void)
(cffi:defcfun (c-glfwinit "glfwInit") :int)
(defun glfwinit ()
  (if (= 0 (c-glfwinit))
      (progn (format t "Failed to initialize GLFW~%")
             nil)
      t))

(defparameter null-pointer (cffi:null-pointer))
(defparameter +GL_FLOAT+ #x1406)

(defparameter +GL-TRUE+ 1)
(defparameter +GL-FALSE+ 0)
(defparameter +GLFW-TRUE+ 1)
(defparameter +GLFW-FALSE+ 0)

(defparameter +GLFW-SAMPLES+ #x0002100D)
(defparameter +GLFW-CONTEXT-VERSION-MAJOR+ #x00022002)
(defparameter +GLFW-CONTEXT-VERSION-MINOR+ #x00022003)
(defparameter +GLFW-OPENGL-FORWARD-COMPAT+ #x00022006)
(defparameter +GLFW-OPENGL-PROFILE+ #x00022008)
(defparameter +GLFW-OPENGL-CORE-PROFILE+ #x00032001)

(defparameter +GL_STATIC_DRAW+ #x88E4)
(defparameter +GL_ARRAY_BUFFER+ #x8892)
(defparameter +GL_COLOR_BUFFER_BIT+ #x00004000)




(cffi:defcfun (c-glfwWindowHint "glfwWindowHint") :void
  (hint :int) (value :int))

;; https://www.glfw.org/faq.html#41---how-do-i-create-an-opengl-30-context
(defun init-glew-and-context ()
  (if (not (glfwinit)) (error "glfwinit failed"))
  (c-glfwwindowhint +glfw-samples+ 2)
  (c-glfwwindowhint +glfw-context-version-major+ 3)
  (c-glfwwindowhint +glfw-context-version-minor+ 3)
  (c-glfwwindowhint +glfw-opengl-forward-compat+ +gl-true+)
  (c-glfwwindowhint +glfw-opengl-profile+ +glfw-opengl-core-profile+))

(cffi:defcfun (c-glfwCreateWindow "glfwCreateWindow") :pointer
  (width :int) (height :int) (title :pointer) (monitor :pointer) (share :pointer))

;; void glfwMakeContextCurrent 	( 	GLFWwindow *  	window	)
(cffi:defcfun (c-glfwMakeContextCurrent "glfwMakeContextCurrent") :void
  (window :pointer))

(cffi:defcfun (c-glfwPollEvents "glfwPollEvents") :void)

(cffi:defcfun (c-glfwSwapBuffers "glfwSwapBuffers") :void
  (window :pointer))

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


