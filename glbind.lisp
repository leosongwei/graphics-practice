(ql:quickload 'cffi)

(cffi:use-foreign-library "libGLEW.so")
(cffi:use-foreign-library "libglfw.so")


;; int glfwInit(void)
(cffi:defcfun (c-glfwinit "glfwInit") :int)
(defun glfwinit ()
  (if (= 0 (c-glfwinit))
      (progn (format t "Failed to initialize GLFW~%")
             nil)
      t))

(defparameter null-pointer (cffi:null-pointer))

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
