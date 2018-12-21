(cffi:use-foreign-library "libGLEW.so")
(cffi:use-foreign-library "libGLU.so")
(cffi:use-foreign-library "libGL.so")
(cffi:use-foreign-library "libGLX.so")
(cffi:use-foreign-library "libSDL2.so")

;; (aref img y x c)
(defmacro with-png-buffer (buffer-name file-name width height &body body)
  (let ((image (gensym))
        (input (gensym))
        (channels (gensym))
        (buffer-size (gensym))
        (x (gensym)) (y (gensym)) (c (gensym)))
    `(let* ((,image (with-open-file (,input ,file-name
                                            :element-type '(unsigned-byte 8))
                      (png:decode ,input)))
            (,width (png:image-width ,image))
            (,height (png:image-height ,image))
            (,channels (png:image-channels ,image))
            (,buffer-size (* ,width ,height ,channels)))
       (cffi:with-foreign-object (,buffer-name :uint8 ,buffer-size)
         (dotimes (,x ,width)
           (dotimes (,y ,height)
             (dotimes (,c ,channels)
               (setf (cffi:mem-aref ,buffer-name :uint8
                                    (+ ,c
                                       (* ,x ,channels)
                                       (* ,y (* ,width ,channels))))
                     (aref ,image (- ,height ,y 1) ,x ,c)))))
         ,@body))))


(defun c-sizeof (type)
  (cffi:foreign-type-size type))

;; GLenum glewErr = glewInit();
(cffi:defcfun (c-glewinit "glewInit") :uint)

(defun float-buffer (float-array)
  (cffi:foreign-alloc :float :initial-contents float-array))
;;(cffi:mem-aref (float-buffer #(1.0 2.0 3.0 4.0)) :float 1) => 2.0

(defun alloc-mat44f ()
  (cffi:foreign-alloc :float :count 16))

(defun update-mat44f-buffer (mat44 buffer)
  ;; row-major
  (dotimes (i 4)
    (dotimes (j 4)
      (let ((pos (+ j (* i 4))))
        (setf (cffi:mem-aref buffer :float pos)
              (aref mat44 i j)))))
  buffer)

(defun cffi-buffer (element-type array)
  (cffi:foreign-alloc element-type :initial-contents array))

(defparameter null-pointer (cffi:null-pointer))

(defmacro defenum (&body list)
  (append
   '(progn)
   (let ((params '())
         (index 0))
     (dolist (e list)
       (if (consp e) (setf index (cadr e)))
       (push `(defparameter ,(if (consp e) (car e) e) ,index) params)
       (incf index))
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
(defparameter +GL_CULL_FACE+ #x0B44)
(defparameter +GL_TEXTURE_2D+ #x0DE1)
(defparameter +GL_UNSIGNED_BYTE+ #x1401)
(defparameter +GL_UNSIGNED_INT+ #x1405)
(defparameter +GL_FLOAT+ #x1406)
(defparameter +GL_NEAREST+ #x2600)
(defparameter +GL_LINEAR+ #x2601)
(defparameter +GL_LINEAR_MIPMAP_LINEAR+ #x2703)
(defparameter +GL_TEXTURE_MAG_FILTER+ #x2800)
(defparameter +GL_TEXTURE_MIN_FILTER+ #x2801)
(defparameter +GL_TRUE+ 1)
(defparameter +GL_FALSE+ 0)
(defparameter +GL_STATIC_DRAW+ #x88E4)
(defparameter +GL_ARRAY_BUFFER+ #x8892)
(defparameter +GL_ELEMENT_ARRAY_BUFFER+ #x8893)
(defparameter +GL_COLOR_BUFFER_BIT+ #x00004000)
(defparameter +GL_DEPTH_BUFFER_BIT+ #x00000100)
(defparameter +GL_LESS+ #x0201)
(defparameter +GL_DEPTH_TEST+ #x0B71)
(defparameter +GL_TRIANGLES+ #x0004)
(defparameter +GL_FRAGMENT_SHADER+ #x8B30)
(defparameter +GL_VERTEX_SHADER+ #x8B31)
(defparameter +GL_VERSION+ #x1F02)
(defparameter +GL_COMPILE_STATUS+ #x8B81)
(defparameter +GL_LINK_STATUS+ #x8B82)
(defparameter +GL_INFO_LOG_LENGTH+ #x8B84)
(defparameter +GL_RGB+ #x1907)
(defparameter +GL_RGBA+ #x1908)
(defparameter +GL_TEXTURE0+ #x84C0)
(defparameter +GL_CURRENT_PROGRAM+ #x8B8D)

;; types
(cffi:defctype :gl-enum :uint)
(cffi:defctype :gl-bitfield :uint)
(cffi:defctype :gl-sizei :int)
(cffi:defctype :gl-boolean :uchar)

;; void glGetIntegerv(GLenum pname, GLint * data);
(cffi:defcfun (c-gl-get-intergerv "glGetIntegerv") :void
  (pname :gl-enum) (data :pointer))

(defun gl-get-integer-v (pname)
  (cffi:with-foreign-object (data :int)
    (c-gl-get-intergerv pname data)
    (cffi:mem-ref data :int)))
;; (list *shader-program-id* (gl-get-integer-v +gl_current_program+))

;; void glFlush(void);
(cffi:defcfun (c-gl-flush "glFlush") :void)
;; void glFinish(void);
(cffi:defcfun (c-gl-flush "glFinish") :void)

;; void glGenVertexArrays(GLsizei n, GLuint *arrays);
(cffi:defcfun (c-gl-gen-vertex-arrays "glGenVertexArrays") :void
  (n :gl-sizei) (arrays-ptr :pointer))

(defun glgen-vertex-array-1 ()
  (cffi:with-foreign-object (new-array :uint)
    (c-gl-gen-vertex-arrays 1 new-array)
    (cffi:mem-ref new-array :uint)))

;; void glBindVertexArray(GLuint array);
(cffi:defcfun (c-gl-bind-vertex-array "glBindVertexArray") :void
  (array-id :uint))

;; typedef int GLsizei;
;; typedef unsigned int GLuint;
;; void glGenBuffers(GLsizei n, GLuint * buffers);
(cffi:defcfun (c-gl-genbuffers "glGenBuffers") :void
  (n :int) (buffers :pointer))
(defun gl-gen-buffer-1 ()
  (cffi:with-foreign-object (new-buffer :uint)
    (c-gl-genbuffers 1 new-buffer)
    (cffi:mem-ref new-buffer :uint)))

;; typedef unsigned int GLenum;
;; void glBindBuffer(GLenum target, GLuint buffer);
(cffi:defcfun (c-gl-bind-Buffer "glBindBuffer") :void
  (target :uint) (buffer :int))

;; void glBufferData(GLenum target, GLsizeiptr size, const GLvoid * data, GLenum usage);
;; glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data),
;;              g_vertex_buffer_data, GL_STATIC_DRAW);
(cffi:defcfun (c-gl-buffer-data "glBufferData") :void
  (target :uint) (size :long) (data :pointer) (usage :uint))

;; void glEnableVertexAttribArray(GLuint index);
(cffi:defcfun (c-gl-enable-vertex-attrib-array "glEnableVertexAttribArray") :void
  (index :uint))

;; void glDisableVertexAttribArray(GLuint index);
(cffi:defcfun (c-gl-disable-vertex-attrib-array "glDisableVertexAttribArray") :void
  (index :uint))

;; typedef unsigned char GLboolean;
;; void glVertexAttribPointer(
;;      GLuint index,
;;  	GLint size,
;;  	GLenum type,
;;  	GLboolean normalized,
;;  	GLsizei stride,
;; 	const GLvoid * pointer);
(cffi:defcfun (c-gl-vertex-attrib-pointer "glVertexAttribPointer") :void
  (index :uint) (size :int) (type :gl-enum)
  (normalized :gl-boolean) (stride :gl-sizei) (pointer :pointer))

;; void glClear(GLbitfield mask);
(cffi:defcfun (c-gl-clear "glClear") :void
  (mask :gl-bitfield))

;; void glClearColor(GLclampf red,  GLclampf green,  GLclampf blue,  GLclampf alpha);
;; typedef float GLclampf;
(cffi:defcfun (c-gl-clear-color "glClearColor") :void
  (red :float) (green :float) (blue :float) (alpha :float))

;; void glEnable(GLenum cap);
(cffi:defcfun (c-gl-enable "glEnable") :void (cap :gl-enum))

;; void glDepthFunc(GLenum func);
(cffi:defcfun (c-gl-depthfunc "glDepthFunc") :void (func :gl-enum))

;; void glDrawArrays(GLenum mode, GLint first, GLsizei count);
(cffi:defcfun (c-gl-draw-arrays "glDrawArrays") :void
  (mode :gl-enum) (first :int) (count :gl-sizei))

;; void glDrawElements(GLenum mode, GLsizei count, GLenum type, const GLvoid * indices);
(cffi:defcfun (c-gl-draw-elements "glDrawElements") :void
  (mode :gl-enum) (count :gl-sizei) (type :gl-enum) (indices-offset :pointer))

;; GLenum glGetError(void);
(cffi:defcfun (c-gl-get-error "glGetError") :gl-enum)

;;  const GLubyte *glGetString(GLenum name);
(cffi:defcfun (c-gl-get-string "glGetString") :pointer
  (name :gl-enum))

;;;; -------------------- shaders -----------------------

;; GLuint glCreateShader(GLenum shaderType);
(cffi:defcfun (c-gl-create-shader "glCreateShader") :uint
  (shader-type :gl-enum))

;; void glShaderSource(GLuint shader, GLsizei count,
;;                     const GLchar **string, const GLint *length);
(cffi:defcfun (c-gl-shader-source "glShaderSource") :void
  (shader-id :uint) (count :gl-sizei) (string-** :pointer) (length-* :pointer))

;; void glCompileShader(GLuint shader);
(cffi:defcfun (c-gl-compile-shader "glCompileShader") :void
  (shader-id :uint))

;; void glGetShaderiv(GLuint shader, GLenum pname, GLint *params);
(cffi:defcfun (c-gl-get-shader-iv "glGetShaderiv") :void
  (shader-id :uint) (parameter-name :gl-enum) (parameters-pointer :pointer))

;; void glGetShaderInfoLog(GLuint shader, GLsizei maxLength, GLsizei *length, GLchar *infoLog);
(cffi:defcfun (c-gl-get-shader-info-log "glGetShaderInfoLog") :void
  (shader-id :uint) (max-length :gl-sizei) (length-pointer :pointer) (log-buffer :pointer))

(defun compile-shader-from-string (shader-type lisp-string)
  "Return shader ID"
  (let ((shader-id (c-gl-create-shader shader-type)))
    (if (= 0 shader-id)
        (error "Unable to create shader"))
    (cffi:with-foreign-string (cstring lisp-string)
      (cffi:with-foreign-object (cstring-p :pointer)
        (setf (cffi:mem-ref cstring-p :pointer) cstring)
        (c-gl-shader-source shader-id 1 cstring-p null-pointer)
        (c-gl-compile-shader shader-id)))
    (cffi:with-foreign-object (compile-status :int)
      (c-gl-get-shader-iv shader-id +GL_COMPILE_STATUS+ compile-status)
      (if (= +GL_FALSE+ (cffi:mem-ref compile-status :int))
          (cffi:with-foreign-object (log-length :int)
            (c-gl-get-shader-iv shader-id +GL_INFO_LOG_LENGTH+ log-length)
            (let ((length (cffi:mem-ref log-length :int)))
              (format t "Shader compile error:~%")
              (format t "Compile log length: ~A~%" length)
              (cffi:with-foreign-object (log-buffer :char length)
                (c-gl-get-shader-info-log shader-id length null-pointer log-buffer)
                (let ((log-string (cffi:foreign-string-to-lisp log-buffer)))
                  ;; (format t "real string length: ~A~%" (length log-string))
                  (format t "Compile log:~%~A~%" log-string)))))))
    shader-id))

;; (let ((shader-id (c-gl-create-shader +gl_vertex_shader+)))
;;   (cffi:with-foreign-string (cstring *vertex-shader-string*)
;;     (cffi:with-foreign-object (cstring-p :pointer)
;;       (setf (cffi:mem-ref cstring-p :pointer) cstring)
;;       (c-gl-shader-source shader-id 1 cstring-p null-pointer)))
;;   shader-id)
;; (c-gl-compile-shader 3)

;; void glDeleteShader(GLuint shader);
(cffi:defcfun (c-gl-delete-shader "glDeleteShader") :void (shader-id :uint))

;; GLuint glCreateProgram(void);
(cffi:defcfun (c-gl-create-program "glCreateProgram") :uint)

;; void glAttachShader(GLuint program, GLuint shader);
(cffi:defcfun (c-gl-attach-shader "glAttachShader") :void
  (program-id :uint) (shader-id :uint))

;; void glDetachShader(GLuint program, GLuint shader);
(cffi:defcfun (c-gl-detach-shader "glDetachShader") :void
  (program-id :uint) (shader-id :uint))

;; void glLinkProgram( 	GLuint program);
(cffi:defcfun (c-gl-link-program "glLinkProgram") :void
  (program-id :uint))

;; void glGetProgramiv(GLuint program, GLenum pname, GLint *params);
(cffi:defcfun (c-gl-get-program-iv "glGetProgramiv") :void
  (program-id :uint) (parameter-name :gl-enum) (parameters-pointer :pointer))


;; void glGetProgramInfoLog(GLuint program, GLsizei maxLength,
;;                          GLsizei *length, GLchar *infoLog);
(cffi:defcfun (c-gl-get-program-info-log "glGetProgramInfoLog") :void
  (program-id :uint) (max-length :gl-sizei) (length-pointer :pointer) (info-log :pointer))

(defun create-program-with-shaders (vs fs)
  (let ((program-id (c-gl-create-program)))
    ;; attach shaders
    (c-gl-attach-shader program-id vs)
    (c-gl-attach-shader program-id fs)
    ;; link
    (c-gl-link-program program-id)
    ;; detach shaders
    (c-gl-detach-shader program-id vs)
    (c-gl-detach-shader program-id fs)
    ;; check if link error
    (cffi:with-foreign-object (link-status :int)
      (c-gl-get-program-iv program-id +GL_LINK_STATUS+ link-status)
      (if (= +GL_FALSE+ (cffi:mem-ref link-status :int))
          (cffi:with-foreign-object (log-length :int)
            (c-gl-get-program-iv program-id +GL_INFO_LOG_LENGTH+ log-length)
            (let ((length (cffi:mem-ref log-length :int)))
              (cffi:with-foreign-object (log-buffer :char length)
                (c-gl-get-program-info-log program-id length null-pointer log-buffer)
                (let ((log-string (cffi:foreign-string-to-lisp log-buffer)))
                  (format t "Shader Link Error:~%")
                  (format t "Link log:~%~A~%" log-string)))))))
    program-id))

;; void glUseProgram(GLuint program);
(cffi:defcfun (c-gl-use-program "glUseProgram") :void
  (program-id :uint))

;; GLint glGetAttribLocation(GLuint program, const GLchar *name);
(cffi:defcfun (c-gl-get-attrib-location "glGetAttribLocation") :int
  (program-id :uint) (name-string :pointer))

;; GLint glGetUniformLocation(GLuint program, const GLchar *name);
(cffi:defcfun (c-gl-get-uniform-location "glGetUniformLocation") :int
  (program-id :uint) (name-string :pointer))
(defun gl-get-uniform-location (program-id name)
  (cffi:with-foreign-string (name-string name)
    (c-gl-get-uniform-location program-id name-string)))

;; void glUniform1i(GLint location, GLint v0);
(cffi:defcfun (c-gl-uniform-1i "glUniform1i") :void
  (location :int) (v0 :int))

;; void glUniform4f(GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3);
(cffi:defcfun (c-gl-uniform-4f "glUniform4f") :void
  (location :int) (v0 :float) (v1 :float) (v2 :float) (v3 :float))

;; void glUniformMatrix4fv(GLint location,
;;        GLsizei count, GLboolean transpose,
;;        const GLfloat *value);
(cffi:defcfun (c-gl-uniform-matrix-4fv "glUniformMatrix4fv") :void
  (location :int) (count :gl-sizei) (transpose :gl-boolean) (value-pointer :pointer))

(defun gl-uniform-mat4fv (uniform-name mat44f &optional (transpose t))
  (let* ((shader-id (gl-get-integer-v +GL_CURRENT_PROGRAM+))
         (location (gl-get-uniform-location shader-id uniform-name))
         (buffer (alloc-mat44f)))
    (update-mat44f-buffer mat44f buffer)
    (c-gl-uniform-matrix-4fv location 1 (if transpose +GL_TRUE+ +GL_FALSE+)
                             buffer)
    (cffi:foreign-free buffer)))

;; void glGenTextures(GLsizei n, GLuint * textures);
(cffi:defcfun (c-gl-gen-textures "glGenTextures") :void
  (n :gl-sizei) (textures-array :pointer))
(defun gl-gen-texute-1 ()
  (cffi:with-foreign-object (texture-id :int)
    (c-gl-gen-textures 1 texture-id)
    (cffi:mem-ref texture-id :int)))

;; void glBindTexture(GLenum target, GLuint texture);
(cffi:defcfun (c-gl-bind-texture "glBindTexture") :void
  (target :gl-enum) (texture :int))

;; void glTexImage2D
;;(GLenum target,
;; GLint level,
;; GLint internalformat,
;; GLsizei width,
;; GLsizei height,
;; GLint border,
;; GLenum format,
;; GLenum type,
;; const GLvoid * data);
;; Example: (https://github.com/JoeyDeVries/LearnOpenGL)
;; glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
(cffi:defcfun (c-gl-tex-image-2d "glTexImage2D") :void
  (target :gl-enum)
  (level :int) ;; mipmap level
  (internal-format :int) ;; OpenGL format: GL_RGB
  (width :gl-sizei) (height :gl-sizei)
  (border :int) ;; set to 0, (deprecated)
  (format :gl-enum) ;; pixel data format
  (type :gl-enum) ;; pixel data unit type
  (data :pointer))

;;  void glActiveTexture(GLenum texture);
(cffi:defcfun (c-gl-active-texture "glActiveTexture") :void (texture :gl-enum))

;; void glTexParameteri(GLenum target, GLenum pname, GLint param);
(cffi:defcfun (c-gl-tex-parameter-i "glTexParameteri") :void
  (target :gl-enum) (pname :gl-enum) (param :int))

;; void glGenerateMipmap(GLenum target);
(cffi:defcfun (c-gl-generate-mipmap "glGenerateMipmap") :void
  (target :gl-enum))
