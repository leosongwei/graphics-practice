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
