(load "../load-libs")

(use-package :3d-matrices)
(use-package :3d-vectors)

;;;; Load this file in and REPL and try each line

;;;; ---------------------------------------
;;;; 3d-vectors

;;;; construct
(vec4 1 2 3 4)
(vec3 1 2 3)
(vec 1 2 3 4)
(vec 1 2)

;;;; ref
(let ((v (vec 1 2 3 4)))
  (setf (vw v) 233)
  (list (vx v) (vy v) (vz v) (vw v)))

(let ((v (vec 1 2 3 4)))
  (with-vec (x y z w) v
    (princ (list x y z w))
    (setf x 233.0))
  v) ;; v is not modified

;;;; compuations
(v+ (vec 1 2 3)
    (vec 1 2 3))
(nv* (vec 1 2) 3)
;; cross product
(vc (vec 1 2 3)
    (vec 3 4 5))
;; dot product
(v. (vec 1 2 3) (vec 1 2 3))
;; get unit vector
(v= (vscale (vec 1 2 3) 1) (vunit (vec 1 2 3)))
(vlength (vec 1 1))
(vdistance (vec 1 0 0) (vec 1 1 0))
;; rotation
(vrot (vrot (vec 1 0 0) +vz+ (* 0.5 pi))
      +vz+ (* 0.5 pi))

;; monification
(let ((v (vec 1 2 3)))
  (nvscale v 2)
  v)
(let ((v (vec 1 2 3)))
  (nvunit v)
  v)
(let ((v (vec 1 2 3)))
  (nv* v 2)
  v)

;;;; --------------------------------------
;;;; 3d-matrices

(mat 1 2 3 4) ;; => mat2
(mat 1 2 3 4 5 6 7 8 9) ;; => mat3
(mat 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) ;; => mat4

(mat2 1) ;; => (MAT2 #(1.0 1.0 1.0 1.0))
(mat2 '(1 2 3 4)) => (MAT2 #(1.0 2.0 3.0 4.0))
(meye 4) ;; => mat4, identity
(mrand 4 4) ;; => mat4, random 0~1
(muniform 3 3 1) ;; mat3, all 1.0
(matn 3 3 1) ;; same as above

;; friendly output
(describe (meye 3))
;; =>
;; 3x3 matrix of type MAT3
;; ┌  +1.000e+0  +0.000e+0  +0.000e+0 ┐
;; │  +0.000e+0  +1.000e+0  +0.000e+0 │
;; └  +0.000e+0  +0.000e+0  +1.000e+0 ┘
(write-matrix (meye 3) t)
;; ┌  +1.000e+0  +0.000e+0  +0.000e+0 ┐
;; │  +0.000e+0  +1.000e+0  +0.000e+0 │
;; └  +0.000e+0  +0.000e+0  +1.000e+0 ┘

;;;; reference
(let ((m (mat4 0)))
  (setf (mcref m 2 1) 233) ;; row:y=2 col:x=1
  (describe m))
;; ┌  +0.000e+0  +0.000e+0  +0.000e+0  +0.000e+0 ┐
;; │  +0.000e+0  +0.000e+0  +0.000e+0  +0.000e+0 │
;; │  +0.000e+0  +2.330e+2  +0.000e+0  +0.000e+0 │
;; └  +0.000e+0  +0.000e+0  +0.000e+0  +0.000e+0 ┘
(let ((m3 (mat3 0)))
  (setf (mcol m3 1) (vec3 33 33 33))
  (setf (mrow m3 2) (vec3 0 0 0))
  (describe m3))
;; ┌  +0.000e+0  +3.300e+1  +0.000e+0 ┐
;; │  +0.000e+0  +3.300e+1  +0.000e+0 │
;; └  +0.000e+0  +0.000e+0  +0.000e+0 ┘

;; computations
(describe (m* (meye 3) 2))
;; ┌  +2.000e+0  +0.000e+0  +0.000e+0 ┐
;; │  +0.000e+0  +2.000e+0  +0.000e+0 │
;; └  +0.000e+0  +0.000e+0  +2.000e+0 ┘
(describe (m* (meye 3) (mat3 3)))
;; ┌  +3.000e+0  +3.000e+0  +3.000e+0 ┐
;; │  +3.000e+0  +3.000e+0  +3.000e+0 │
;; └  +3.000e+0  +3.000e+0  +3.000e+0 ┘
(let ((m1 (mat 1 2
               3 4))
      (m2 (mat 5 6
               7 8)))
  (describe (m* m1 m2))
  ;; ┌  +1.900e+1  +2.200e+1 ┐
  ;; └  +4.300e+1  +5.000e+1 ┘
  (describe (m* m2 m1)))
;; ┌  +2.300e+1  +3.400e+1 ┐
;; └  +3.100e+1  +4.600e+1 ┘

;; translations
(let ((mt (mtranslation (vec 1 2 3))))
  ;; ┌  +1.000e+0  +0.000e+0  +0.000e+0  +1.000e+0 ┐
  ;; │  +0.000e+0  +1.000e+0  +0.000e+0  +2.000e+0 │
  ;; │  +0.000e+0  +0.000e+0  +1.000e+0  +3.000e+0 │
  ;; └  +0.000e+0  +0.000e+0  +0.000e+0  +1.000e+0 ┘
  (princ (m* mt (vec 1 2 3 0)))  ;; => (VEC4 1.0 2.0 3.0 0.0), vector can not be translated
  (princ (m* mt (vec 1 2 3 1)))) ;; => (VEC4 2.0 4.0 6.0 1.0), vetex can

(let ((m (meye 4)))
  (m*
   (nmtranslate (meye 4) (vec 1 1 1)) ;; (3) add 1 to each, -3,3,7
   (nmrotate (meye 4) (vec 0 0 1) (* 0.5 pi)) ;; (2) rotate anticlockwise to -4,2,6
   (nmscale (meye 4) (vec 2 2 2)) ;; (1) scale to 2,4,6
   (vec 1 2 3 1)))

(let ((m (mtranslation (vec 1 1 1))))
  (nmrotate m (vec 0 0 1) (* 0.5 pi))
  (nmscale m (vec 2 2 2))
  (m* m (vec 1 2 3 1))) ;; same as above, keep modifying the m in place

;;;; ----------------------------------------
;;;; CFFI

(3d-cffi:with-c-mat (cm (meye 4))
  (m= (meye 4)
      (mat4 (loop for i from 0 to 15
                  collect (cffi:mem-aref cm :float i)))))

(3d-cffi:with-c-vec4 (cv (vec 1 2 3 4))
  (loop for i from 0 to 3
        collect (cffi:mem-aref cv :float i)))
