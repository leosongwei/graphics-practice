;;(declaim (optimize (speed 3) (safety 0)))

;; vertex
;;   3f: model space coord
;;   3f: normal
;;   3f: color
;;   2f: texture coord

(defun copy-float-array (array)
  (declare (type (simple-array single-float (*)) array)
           (optimize (speed 3)))
  (let ((a (make-array (length array) :element-type 'single-float)))
    (dotimes (i (length array))
      (setf (aref a i) (aref array i)))
    a))

(defun make-mat-from-vecs (&rest vecs)
  (let ((a (make-array `(,(length vecs) ,(length (nth 0 vecs))))))
    (dotimes (r (length vecs))
      (let* ((v (nth r vecs))
             (len (length v)))
        (dotimes (c len)
          (setf (aref a r c) (aref v c)))))
    a))
;; (mat-mul #2a((1 2) (3 4)) (make-mat #(1 2 3) #(4 5 6)))

(defun flat-mat (mat)
  (let* ((row (array-dimension mat 0))
         (col (array-dimension mat 1))
         (new (make-array (* row col) :element-type 'single-float)))
    (dotimes (r row)
      (dotimes (c col)
        (setf (aref new (+ c (* r col))) (aref mat r c))))
    new))
;; (flat-mat #2a((1.0 2.0) (3.0 4.0)))

;; ---------------------------------

(defun mat-add (mat-a mat-b)
  (let* ((a mat-a)
         (b mat-b)
         (row (array-dimension a 0))
         (col (array-dimension a 1))
         (new (make-array `(,row ,col) :element-type 'single-float)))
    (dotimes (r row)
      (dotimes (c col)
        (setf (aref new r c) (+ (aref a r c) (aref b r c)))))
    new))

(defun mat-mul (a b)
  (let* ((row (array-dimension a 0))
         (col (array-dimension b 1))
         (col-row (if (= (array-dimension a 1) (array-dimension b 0))
                      (array-dimension a 1)
                      (progn
                        (error "mat-mul: arrays not match")
                        -233)))
         (m (make-array `(,row ,col) :element-type 'single-float)))
    (dotimes (r row)
      (dotimes (c col)
        (setf (aref m r c)
              (let ((sum 0.0))
                (dotimes (j col-row)
                  (incf sum (* (aref a r j) (aref b j c))))
                sum))))
    m))

(defun mat-mul-x (x mat)
  (let ((new (make-array `(,(array-dimension mat 0)
                            ,(array-dimension mat 1))
                         :element-type 'single-float)))
    (dotimes (r (array-dimension mat 0))
      (dotimes (c (array-dimension mat 1))
        (setf (aref new r c) (* x (aref mat r c)))))
    new))
;; (mul-x-mat 2.0 #2a((1 2 3) (3 2 1)))

(defun mul-33-33 (a b)
  (let ((m (make-array '(3 3) :element-type 'single-float)))
    (dotimes (r 3)
      (dotimes (c 3)
        (setf (aref m r c)
              (+ (* (aref a r 0) (aref b 0 c))
                 (* (aref a r 1) (aref b 1 c))
                 (* (aref a r 2) (aref b 2 c))))))
    m))

(defun mul-44-44 (a b)
  (let ((m (make-array '(4 4) :element-type 'single-float)))
    (dotimes (r 4)
      (dotimes (c 4)
        (let ((sum 0.0))
          (dotimes (j 4)
            (incf sum (* (aref a r j) (aref b j c))))
          (setf (aref m r c) sum))))
    m))

(defun load-i-33 ()
  (make-array '(3 3) :element-type 'single-float
              :initial-contents '((1.0 0.0 0.0)
                                  (0.0 1.0 0.0)
                                  (0.0 0.0 1.0))))

(defun load-i-44 ()
  (make-array '(4 4) :element-type 'single-float
              :initial-contents '((1.0 0.0 0.0 0.0)
                                  (0.0 1.0 0.0 0.0)
                                  (0.0 0.0 1.0 0.0)
                                  (0.0 0.0 0.0 1.0))))

(defun mul-33-v3 (a v)
  (let ((vec (make-array '(3) :element-type 'single-float)))
    (dotimes (r 3)
      (setf (aref vec r)
            (let ((sum 0.0))
              (dotimes (j 3)
                (incf sum (* (aref a r j) (aref v j))))
              sum)))
    vec))

(defun mul-44-v4 (a v)
  (declare (optimize (speed 3))
           (type (simple-array single-float (4 4)) a)
           (type (simple-array single-float (4)) v))
  (let ((vec (make-array '(4) :element-type 'single-float)))
    (dotimes (r 4)
      (setf (aref vec r)
            (let ((sum 0.0))
              (declare (type single-float sum))
              (dotimes (j 4)
                (incf sum (* (aref a r j) (aref v j))))
              sum)))
    vec))

#|
(let ((m33 #2a((1.0 2.0 3.0)
               (4.0 5.0 6.0)
               (7.0 8.0 9.0)))
      (v3  #1a(1.0 2.0 3.0))
      (m44 #2a((1.0 2.0 3.0 4.0)
               (4.0 3.0 2.0 1.0)
               (1.0 2.0 3.0 4.0)
               (5.0 6.0 7.0 8.0)))
      (v4 #1a(1.0 2.0 3.0 4.0)))
  (mul-33-v3 m33 v3)
  (mul-44-44 m44 m44)
  (mul-44-v4 m44 v4)
  (mat-mul m44 m44))
|#


#|
M : matrix([1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]);
V : transpose(matrix([1,2,3]));
M4 : matrix(
    [1, 2, 3, 4],
    [4, 3, 2, 1],
    [1, 2, 3, 4],
    [5, 6, 7, 8]);
V4 : transpose(matrix([1.0, 2.0, 3.0, 4.0]));
|#

(defconstant M_PI 3.14159265358979323846)

(defun 3d-trans-mat (x y z)
  (make-array '(4 4) :element-type 'single-float
              :initial-contents `((1.0 0.0 0.0 ,x)
                                  (0.0 1.0 0.0 ,y)
                                  (0.0 0.0 1.0 ,z)
                                  (0.0 0.0 0.0 1.0))))

(defun 3d-rotate-x (degree)
  (let* ((theta (* (/ degree 180) M_PI))
         (sin (sin theta))
         (cos (cos theta)))
    (make-array '(4 4) :element-type 'single-float
                :initial-contents
                `((1.0  0.0   0.0      0.0)
                  (0.0  ,cos  ,(- sin) 0.0)
                  (0.0  ,sin  ,cos     0.0)
                  (0.0  0.0   0.0      1.0)))))

(defun 3d-rotate-y (degree)
  (let* ((theta (* (/ degree 180) M_PI))
         (sin (sin theta))
         (cos (cos theta)))
    (make-array '(4 4) :element-type 'single-float
                :initial-contents
                `((,cos      0.0  ,sin  0.0)
                  (0.0       1.0  0.0   0.0)
                  (,(- sin)  0.0  ,cos  0.0)
                  (0.0       0.0  0.0   1.0)))))

(defun 3d-rotate-z (degree)
  (let* ((theta (* (/ degree 180) M_PI))
         (sin (sin theta))
         (cos (cos theta)))
    (make-array '(4 4) :element-type 'single-float
                :initial-contents
                `((,cos  ,(- sin)  0.0  0.0)
                  (,sin  ,cos      0.0  0.0)
                  (0.0   0.0       1.0  0.0)
                  (0.0   0.0       0.0  1.0)))))

(defun 3d-scale (s)
  (make-array '(4 4) :element-type 'single-float
              :initial-contents `((,s  0.0 0.0 0.0)
                                  (0.0 ,s  0.0 0.0)
                                  (0.0 0.0 ,s  0.0)
                                  (0.0 0.0 0.0 1.0))))

(defun frustum-mat (degree-y aspect znear zfar)
  ;; https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPerspective.xml
  ;; fov-y: angle in degree
  ;; aspect: x/y
  ;; output: project vertices into view space, with w.
  ;; **Z will be resulted in positive**
  (let* ((fov-y (* (/ degree-y 180) M_PI))
         (f (/ 1 (tan (/ fov-y 2))))
         (a (/ (+ zfar znear) (- znear zfar)))
         (b (/ (* 2 zfar znear) (- znear zfar))))
    (make-array '(4 4) :element-type 'single-float
                :initial-contents
                `((,(/ f aspect)  0.0   0.0  0.0)
                  (0.0            ,f    0.0  0.0)
                  (0.0            0.0   ,a   ,b)
                  (0.0            0.0  -1.0  0.0)))))

(defun ortho-mat (w h far)
  "
  -----------------------
  |                      |
  |                      |
  |           . (0, 0)   | h
  |                      |
  |                      |
  -----------------------.
  w           (w/2, -h/2)
  near plane = 0, far plane = far"
  (declare (type single-float w h far))
  (let* ((2/w (float (/ 2 w)))
         (2/h (float (/ 2 h)))
         (1/far (float (/ -1 far))))
    (make-array '(4 4) :element-type 'single-float
                :initial-contents
                `((,2/w 0.0 0.0 0.0)
                  (0.0 ,2/h 0.0 0.0)
                  (0.0 0.0 ,1/far 0.0)
                  (0.0 0.0 0.0 1.0)))))

;; ---------------------------------------------------------
;; Vector Calculation
;; ---------------------------------------------------------
;; vec2
;; (defun make-vec4 (x y z w)
;;   (make-array 4 :element-type 'single-float
;;               :initial-contents `(,x ,y ,z ,w)))

(defmacro make-vec4 (x y z w)
  (let ((n0 (gensym)) (n1 (gensym))
        (n2 (gensym)) (n3 (gensym)))
    `(let ((,n0 ,x) (,n1 ,y)
           (,n2 ,z) (,n3 ,w))
      (make-array 4 :element-type 'single-float
               :initial-contents (list ,n0 ,n1 ,n2 ,n3)))))

(defun vec4+ (vec1 vec2)
  (make-array 4 :element-type 'single-float
              :initial-contents
              `(,(+ (aref vec1 0) (aref vec2 0))
                 ,(+ (aref vec1 1) (aref vec2 1))
                 ,(+ (aref vec1 2) (aref vec2 2))
                 ,(+ (aref vec1 3) (aref vec2 3)))))

(defun vec4- (vec1 vec2)
  (make-array 4 :element-type 'single-float
              :initial-contents
              `(,(- (aref vec1 0) (aref vec2 0))
                 ,(- (aref vec1 1) (aref vec2 1))
                 ,(- (aref vec1 2) (aref vec2 2))
                 ,(- (aref vec1 3) (aref vec2 3)))))

(defun vec4* (vec n)
  (make-array 4 :element-type 'single-float
              :initial-contents
              `(,(* (aref vec 0) n)
                 ,(* (aref vec 1) n)
                 ,(* (aref vec 2) n)
                 ,(* (aref vec 3) n))))

(defun vec4/ (vec n)
  (make-array 4 :element-type 'single-float
              :initial-contents
              `(,(/ (aref vec 0) n)
                 ,(/ (aref vec 1) n)
                 ,(/ (aref vec 2) n)
                 ,(/ (aref vec 3) n))))

(defun vec4->vec3 (vec4)
  (make-array 3 :element-type 'single-float
              :initial-contents `(,(aref vec4 0)
                                   ,(aref vec4 1)
                                   ,(aref vec4 2))))
;; ---------------------------------------------------------
;; vec3

(defun make-vec3 (x y z)
  (make-array 3 :element-type 'single-float
              :initial-contents `(,x ,y ,z)))

(defun vec3 (vec)
  (declare (type (SIMPLE-ARRAY SINGLE-FLOAT *) vec))
  (if (>= (length vec) 3)
      (make-array 3 :element-type 'single-float
                  :initial-contents `(,(aref vec 0)
                                       ,(aref vec 1)
                                       ,(aref vec 2)))
      (let ((array (make-array 3 :element-type 'single-float)))
        (dotimes (i (length vec))
          (setf (aref array i) (aref vec i)))
        array)))

(defun vec3+ (vec1 vec2)
  (make-array 3 :element-type 'single-float
              :initial-contents
              `(,(+ (aref vec1 0) (aref vec2 0))
                 ,(+ (aref vec1 1) (aref vec2 1))
                 ,(+ (aref vec1 2) (aref vec2 2)))))

(defun vec3- (vec1 vec2)
  (make-array 3 :element-type 'single-float
              :initial-contents
              `(,(- (aref vec1 0) (aref vec2 0))
                 ,(- (aref vec1 1) (aref vec2 1))
                 ,(- (aref vec1 2) (aref vec2 2)))))

(defun vec3* (vec n)
  (make-array 3 :element-type 'single-float
              :initial-contents
              `(,(* (aref vec 0) n)
                 ,(* (aref vec 1) n)
                 ,(* (aref vec 2) n))))

(defun vec3/ (vec n)
  (make-array 3 :element-type 'single-float
              :initial-contents
              `(,(/ (aref vec 0) n)
                 ,(/ (aref vec 1) n)
                 ,(/ (aref vec 2) n))))

(defmacro ndc-x (ndc)
  `(aref ,ndc 0))

(defmacro ndc-y (ndc)
  `(aref ,ndc 1))

(defmacro ndc-z (ndc)
  `(aref ,ndc 2))

(defun vec3-normalize (vec)
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) vec))
  (let* ((length (sqrt (+ (expt (aref vec 0) 2)
                          (expt (aref vec 1) 2)
                          (expt (aref vec 2) 2)))))
    (make-array 3 :element-type 'single-float
                :initial-contents
                `(,(/ (aref vec 0) length)
                   ,(/ (aref vec 1) length)
                   ,(/ (aref vec 2) length)))))
;; (type-of
;; (make-array 3 :element-type 'single-float
;;             :initial-element 0.0))

(defun vec3-dot (vec1 vec2)
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) vec1 vec2))
  (+ (* (aref vec1 0) (aref vec2 0))
     (* (aref vec1 1) (aref vec2 1))
     (* (aref vec1 2) (aref vec2 2))))

(defun vec3-clamp (vec3 &optional (lb 0.0) (ub 1.0))
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) vec3)
           (type single-float lb ub))
  (make-array 3 :element-type 'single-float
              :initial-contents
              `(,(max lb (min (aref vec3 0) ub))
                 ,(max lb (min (aref vec3 1) ub))
                 ,(max lb (min (aref vec3 2) ub)))))

(defun vec3-reflection (vec normal)
  (vec3- vec (vec3* normal (* 2.0 (vec3-dot vec normal)))))
;;
;; p0   |   p2   (vec3-reflection p0->p1 normal)
;;  \   |   /    > p1->p2
;;   \  |  /     ;; "normal" should be normalized
;;    \ | /
;; ==== p1 ===
;; (vec3-reflection (make-vec3 1.0 -1.0 0.0)
;;                  (make-vec3 0.0 1.0 0.0))
;; > #(1.0 1.0 0.0)

;; ---------------------------------------------------------
;; vec2
(defun vec2+ (vec1 vec2)
  (make-array 2 :element-type 'single-float
              :initial-contents
              `(,(+ (aref vec1 0) (aref vec2 0))
                 ,(+ (aref vec1 1) (aref vec2 1)))))

(defun vec2- (vec1 vec2)
  (make-array 2 :element-type 'single-float
              :initial-contents
              `(,(- (aref vec1 0) (aref vec2 0))
                 ,(- (aref vec1 1) (aref vec2 1)))))

(defun vec2* (vec n)
  (make-array 2 :element-type 'single-float
              :initial-contents
              `(,(* (aref vec 0) n)
                 ,(* (aref vec 1) n))))

(defun vec2/ (vec n)
  (make-array 2 :element-type 'single-float
              :initial-contents
              `(,(/ (aref vec 0) n)
                 ,(/ (aref vec 1) n))))

(defun make-vec2 (x y)
  (make-array 2 :element-type 'single-float
              :initial-contents `(,x ,y)))

;; (time
;;  (dotimes (i 10000000000)
;;    (let ((a (make-array 4 :element-type 'single-float
;;                         :initial-contents `(,(float i) ,(+ 1.0 i) ,(+ 2.0 i) ,(+ 3.0 i)))))
;;      a)))
;; (time
;;  (dotimes (i 10000000000)
;;    (let* ((n0 (float i))
;;           (n1 (+ 1.0 i))
;;           (n2 (+ 2.0 i))
;;           (n3 (+ 3.0 i))
;;           (a (make-array 4 :element-type 'single-float
;;                          :initial-contents (list n0 n1 n2 n3))))
;;      a)))
;;
;; (time
;;  (dotimes (i 1000000000) ;; 10x slower
;;    (let ((a (make-array 4 :element-type 'single-float :initial-element 0.0)))
;;      (setf (aref a 0) (float i))
;;      (setf (aref a 1) (+ 1.0 i))
;;      (setf (aref a 2) (+ 2.0 i))
;;      (setf (aref a 3) (+ 3.0 i))
;;      a)))

;; other vectors
(defun make-vec9 ()
  (make-array 9 :element-type 'single-float))

;; -------------------------------------------

(defmacro interpolate-macro (m+ m* m- a b pt)
  `(,m+ ,a (,m* (,m- ,b ,a) ,pt)))

(defmacro interp-m (m+ m* m- a b pt)
  `(,m+ ,a (,m* (,m- ,b ,a) ,pt)))

(defmacro interp-num (a b pt)
  `(interp-m + * - ,a ,b ,pt))

(defun interpolate-vec3 (v0 v1 pt)
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) v0 v1)
           (type single-float pt))
  (let ((x0 (aref v0 0)) (y0 (aref v0 1)) (z0 (aref v0 2))
        (x1 (aref v1 0)) (y1 (aref v1 1)) (z1 (aref v1 2)))
    (make-vec3 (interp-num x0 x1 pt)
               (interp-num y0 y1 pt)
               (interp-num z0 z1 pt))))

(defmacro grad (a b len) ;; a->b, length
  `(/ (- ,b ,a) ,len))

;;(declaim (inline dvertex))

(defun dvec3 (v0 v1 length)
  "gradient: (v1-v0)/length"
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) v0 v1)
           (type integer length))
  (let ((x0 (aref v0 0)) (y0 (aref v0 1)) (z0 (aref v0 2))
        (x1 (aref v1 0)) (y1 (aref v1 1)) (z1 (aref v1 2)))
    (make-vec3 (grad x0 x1 length)
               (grad y0 y1 length)
               (grad z0 z1 length))))

;;(declaim (inline vertex+f))

