(in-package #:cl-user)

(defpackage #:stb
  (:use #:cl
        #:cffi)
  (:export #:stbi-load
           #:stbi-image-free
           #:with-stbi-load
           #:stbi-set-flip-vertically-on-load))

(in-package :stb)

;;;; (cffi:use-foreign-library "libstb.so.0")

(defcfun (c-stbi_set_flip_vertically_on_load "stbi_set_flip_vertically_on_load") :void
  (should-flip :int))

(defcfun (c-stbi-load "stbi_load") :pointer
  (filename :string) (width :pointer) (height :pointer) (file-channels-number :pointer) (forced-channels-number :int))

(defcfun (c-stbi-load-from-memory "stbi_load_from_memory") :pointer
  (buffer :pointer) (length :int) (height :pointer) (width :pointer)
  (channels-number :pointer) (forced-channels-number :int))

(defcfun (c-stbi-image-free "stbi_image_free") :void
  (image-pointer :pointer))

(defun stbi-load (filename &optional (forced-channels-number 0))
  "=> '(pointer width height file-channels)"
  (cffi:with-foreign-objects ((cw :int) (ch :int) (c-channels :int))
    (cffi:with-foreign-string (c-filename filename)
      (let ((image-pointer
              (c-stbi-load c-filename cw ch c-channels forced-channels-number)))
        (list image-pointer
              (mem-ref cw :int)
              (mem-ref ch :int)
              (mem-ref c-channels :int))))))
;;;; (stbi-load "/home/leo/dev/graphics-practice/resources/th06.png")

(defun stbi-image-free (image-pointer)
  (c-stbi-image-free image-pointer))

(defmacro with-stbi-load ((pointer-name width-name height-name channels-name) filename
                          &body body)
  `(destructuring-bind (,pointer-name ,width-name ,height-name ,channels-name)
       (stbi-load ,filename 0)
     (unwind-protect
          (progn ,@body)
       (stbi-image-free ,pointer-name))))
;; (with-stbi-load (p w h channels) "/home/leo/dev/graphics-practice/resources/th06.png"
;;   (print (list p w h channels)))

(defun stbi-set-flip-vertically-on-load (flag)
  (ecase flag
    (t (c-stbi_set_flip_vertically_on_load 1))
    (nil (c-stbi_set_flip_vertically_on_load 0))))
