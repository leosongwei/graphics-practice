(defun float-buffer (float-array)
  (cffi:foreign-alloc :float :initial-contents float-array))

(defmacro with-float-buffer ((name float-array) &body body)
  `(let ((,name (float-buffer ,float-array)))
     (unwind-protect (progn ,@body)
       (cffi:foreign-free ,name))))
