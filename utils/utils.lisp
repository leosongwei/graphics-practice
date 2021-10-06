(defun float-buffer (float-array)
  (cffi:foreign-alloc :float :initial-contents float-array))

(defmacro with-float-buffer ((name float-array) &body body)
  `(let ((,name (float-buffer ,float-array)))
     (unwind-protect (progn ,@body)
       (cffi:foreign-free ,name))))

(defmacro with-c-buffer ((name lisp-array element-c-type) &body body)
  `(let ((,name (cffi:foreign-alloc ,element-c-type
                                    :initial-contents ,lisp-array)))
     (unwind-protect (progn ,@body)
       (cffi:foreign-free ,name))))

(cffi:foreign-type-size '%gl:boolean)

(defun gl-type-to-cffi-type (gl-type)
  (ecase gl-type
    (:float :float)
    (:double :double)
    (:int :int32)
    (:unsigned-int :uint32)
    (:bool :uint8)
    (:byte :int8)
    (:short :int16)
    (:ushort :uint16)
    (:int64 :int64)
    (:uint64 :uint64)))

(defun make-vao (vertex-array indicies-array attrib-lengths-and-gl-types)
  (let ((vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer))
        (ebo (gl:gen-buffer)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (with-c-buffer (buffer vertex-array :float)
      (%gl:buffer-data :array-buffer (* (length vertex-array) (cffi:foreign-type-size :float))
                       buffer :static-draw))
    (gl:bind-buffer :element-array-buffer ebo)
    (with-c-buffer (buffer indicies-array :uint32)
      (%gl:buffer-data :element-array-buffer (* (length indicies-array) (cffi:foreign-type-size :uint32))
                       buffer :static-draw))
    (let ((attrib-index 0)
          (offset 0)
          (attrib-total-size
            (let ((size 0))
              (dolist (attrib-length-and-gl-type attrib-lengths-and-gl-types size)
                (destructuring-bind (attrib-length gl-type) attrib-length-and-gl-type
                  (setf size (+ size
                                (* attrib-length (cffi:foreign-type-size
                                                  (gl-type-to-cffi-type gl-type))))))))))
      (mapcar (lambda (attrib-length-and-gl-type)
                (destructuring-bind (attrib-length gl-type) attrib-length-and-gl-type
                  (let* ((cffi-type (gl-type-to-cffi-type gl-type))
                         (attrib-size (* attrib-length (cffi:foreign-type-size cffi-type))))
                    (gl:vertex-attrib-pointer attrib-index attrib-length
                                              gl-type nil attrib-total-size
                                              (cffi:make-pointer offset))
                    (setf offset (+ offset attrib-size))
                    (gl:enable-vertex-attrib-array attrib-index)
                    (incf attrib-index))))
              attrib-lengths-and-gl-types))
    vao))

(defun make-2d-texture (filename)
  (let* ((gl-texture (gl:gen-texture)))
    (gl:bind-texture :texture-2d gl-texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (stb:stbi-set-flip-vertically-on-load t)
    (stb:with-stbi-load (image-pointer w h channels) filename
      channels ;; ignore
      (gl:tex-image-2d :texture-2d 0 :rgb w h 0 :rgb :unsigned-byte image-pointer))
    (gl:generate-mipmap :texture-2d)
    gl-texture))
