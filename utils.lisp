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
