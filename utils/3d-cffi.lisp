;; (ql:quickload '3d-matrices)
;; (ql:quickload 'cffi)
(in-package #:cl-user)
(defpackage #:3d-cffi
  (:nicknames #:3c)
  (:use #:cl
        #:cffi
        #:3d-matrices
        #:3d-vectors)
  (:export #:with-c-vec2
           #:with-c-vec3
           #:with-c-vec4
           #:with-c-mat))

(in-package :3d-cffi)

(defmacro with-c-vec2 ((cvec-name vec) &body body)
  `(let ((,cvec-name (foreign-alloc :float :count 2)))
     (unwind-protect
          (progn
            (setf (mem-aref ,cvec-name :float 0) (vx ,vec)
                  (mem-aref ,cvec-name :float 1) (vy ,vec))
            ,@body)
       (foreign-free ,cvec-name))))

(defmacro with-c-vec3 ((cvec-name vec) &body body)
  `(let ((,cvec-name (foreign-alloc :float :count 3)))
     (unwind-protect
          (progn
            (setf (mem-aref ,cvec-name :float 0) (vx ,vec)
                  (mem-aref ,cvec-name :float 1) (vy ,vec)
                  (mem-aref ,cvec-name :float 2) (vz ,vec))
            ,@body)
       (foreign-free ,cvec-name))))

(defmacro with-c-vec4 ((cvec-name vec) &body body)
  `(let ((,cvec-name (foreign-alloc :float :count 4)))
     (unwind-protect
          (progn
            (setf (mem-aref ,cvec-name :float 0) (vx ,vec)
                  (mem-aref ,cvec-name :float 1) (vy ,vec)
                  (mem-aref ,cvec-name :float 2) (vz ,vec)
                  (mem-aref ,cvec-name :float 3) (vw ,vec))
            ,@body)
       (foreign-free ,cvec-name))))

;; 3d-matrices uses column major array, thus can directly give it to GL
(defmacro with-c-mat ((cmat-name mat) &body body)
  `(let ((,cmat-name (foreign-alloc :float :initial-contents (marr ,mat))))
     (unwind-protect
          (progn ,@body)
       (foreign-free ,cmat-name))))
