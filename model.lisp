(defun read-text-into-line-list (file-path)
  (with-open-file (stream file-path :direction :input
                          :if-does-not-exist :error)
    (let ((list nil))
      (do ((line (read-line stream nil nil) (read-line stream nil nil)))
          ((null line) nil)
        (push line list))
      (reverse list))))

(defun split-string-with-char (string &optional (char #\Space))
  (let ((list nil)
        (collect (make-dynamic-string)))
    (dotimes (i (length string))
      (let ((c (aref string i)))
        (if (not (char= c char))
            (vector-push-extend c collect)
            (progn (push collect list)
                   (setf collect (make-dynamic-string))))))
    (if (> (length collect) 0) (push collect list))
    (reverse list)))
;;(split-string-with-char "sadfsad/sdfasdfa/asfd" #\/)

(defun read-wavefront-obj-file (file-path)
  (let ((line-list (read-text-into-line-list file-path))
        (vertex-list nil)
        (tex-coord-list nil)
        (normal-list nil)
        (face-list nil))
    (dolist (string line-list)
      (let ((list (split-string-with-char string)))
        (cond ((string= "v" (car list))
               ;; v 0.123 0.234 0.345
               (if (= 3 (length (cdr list)))
                   ;; (0.123 0.234 0.345 1.0)
                   (push (mapcar #'float
                                 (mapcar
                                  #'parse-number:parse-number
                                  (append (cdr list) '("1.0"))))
                         vertex-list)
                   (error (format nil
                                  "read-wavefront-obj-file: unknown vertex format: ~A"
                                  string))))
              ((string= "vt" (car list))
               ;; vt 0.500 1
               (if (= 2 (length (cdr list)))
                   ;; #(0.5 1.0)
                   (push (mapcar #'float
                                 (mapcar
                                  #'parse-number:parse-number
                                  (cdr list)))
                         tex-coord-list)
                   (error (format nil
                                  "read-wavefront-obj-file: unknown tex coord format: ~A"
                                  string))))
              ((string= "vn" (car list))
               ;; vn 0.707 0.000 0.707
               (if (= 3 (length (cdr list)))
                   ;; #(0.707 0.000 0.707)
                   (push (mapcar #'float
                                 (mapcar
                                  #'parse-number:parse-number
                                  (append (cdr list) '("0.0"))))
                         normal-list)
                   (error (format nil
                                  "read-wavefront-obj-file: unknown normal format: ~A"
                                  string))))
              ((string= "f" (car list))
               ;; f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3
               ;; f 6/4/1 3/5/3 7/6/5
               ;; 1-indexed
               (if (= 3 (length (cdr list)))
                   ;; ((6 4 1) (3 5 3) (7 6 5))
                   (push (mapcar (lambda (string)
                                   (mapcar #'parse-integer
                                           (split-string-with-char string #\/)))
                                 (cdr list))
                         face-list)
                   (error (format nil
                                  "read-wavefront-obj-file: unknown face format: ~A"
                                  string)))))))
    (values (reverse vertex-list)
            (reverse tex-coord-list)
            (reverse normal-list)
            (reverse face-list))))

;; (multiple-value-bind (vertices tex-coords normals faces)
;;     (read-wavefront-obj-file #p"test.obj")
;;   (list :vertices vertices :tex-coords tex-coords :normals normals :faces faces))

;; model: an arry of triangles

(defun list-2-array (list)
  (let* ((length (length list))
         (array (make-array length))
         (index 0))
    (dolist (e list)
      (setf (aref array index) e)
      (incf index))
    array))

(defun make-array-list (length list)
  (make-array length
              :element-type 'single-float
              :initial-contents list))
;;(make-array-list 4 '(1.0 2.0 3.0 4.0))

(defstruct modelmesh
  vertices  ;;; #(float-vec4 ...)
  tex-coords ;;; #(float-vec4 ...)
  normals ;;; #(float-vec4 ...)
  faces ;;; #(face),
  ;; face:#(int-vec3 int-vec3 int-vec3)
  ;;        coords   uvs      normals
  ;; vertex attribute index, 0-indexed, not like wavefront!!
  tb ;; tangent / bitangent, (2 3) matrix, one to one with each vertex
  )

(defun make-model-from-wave-front (vertices tex-coords normals faces)
  (make-modelmesh :vertices
                  (let* ((vectors
                          (mapcar (lambda (list)
                                    (make-array
                                     4 :element-type 'single-float
                                     :initial-contents list))
                                  vertices)))
                    (make-array (length vectors)
                                :initial-contents vectors))
                  :tex-coords
                  (let* ((vectors
                          (mapcar (lambda (list)
                                    (make-array
                                     2 :element-type 'single-float
                                     :initial-contents list))
                                  tex-coords)))
                    (make-array (length vectors)
                                :initial-contents vectors))
                  :normals
                  (let* ((vectors
                          (mapcar (lambda (list)
                                    (make-array
                                     4 :element-type 'single-float
                                     :initial-contents list))
                                  normals)))
                    (make-array (length vectors)
                                :initial-contents vectors))
                  :faces
                  (let* ((vectors
                          (mapcar (lambda (list)
                                    (make-array ;; vertices
                                     3 :initial-contents
                                     (mapcar (lambda (attrib-index)
                                               (make-array ;; vertex attri index
                                                ;; 1-indexed -> 0-indexed
                                                3 :element-type 'integer
                                                :initial-contents
                                                (mapcar (lambda (x) (1- x)) attrib-index)))
                                             list)))
                                  faces)))
                    (make-array (length vectors)
                                :initial-contents vectors))))

;;(multiple-value-bind (vertices tex-coords normals faces)
;;    (read-wavefront-obj-file #p"test.obj")
;;  (list :vertices vertices :tex-coords tex-coords :normals normals :faces faces))
;;   ;;(make-model-from-wave-front vertices tex-coords normals faces))

(defun wavefront-file-to-modelmesh (file-path)
  (multiple-value-bind (vertices tex-coords normals faces)
      (read-wavefront-obj-file file-path)
    (make-model-from-wave-front vertices tex-coords normals faces)))
;;(wavefront-file-to-modelmesh #p"test.obj")
;; #S(MODELMESH
;;    :VERTICES #(#(0.123 0.234 0.345 1.0) #(0.123 0.234 0.345 1.0)
;;                #(0.123 0.234 0.345 1.0) #(0.123 0.234 0.345 1.0)
;;                #(0.123 0.234 0.345 1.0) #(0.123 0.234 0.345 1.0))
;;    :TEX-COORDS #(#(0.5 1.0) #(0.5 1.0) #(0.5 1.0) #(0.5 1.0) #(0.5 1.0)
;;                  #(0.5 1.0))
;;    :NORMALS #(#(0.707 0.0 0.707 0.0) #(0.707 0.0 0.707 0.0)
;;               #(0.707 0.0 0.707 0.0) #(0.707 0.0 0.707 0.0)
;;               #(0.707 0.0 0.707 0.0) #(0.707 0.0 0.707 0.0))
;;    :FACES #(#(#(0 1 2) #(1 1 1) #(2 2 2)) #(#(3 3 3) #(4 4 4) #(5 5 5))))

;; (defun build-vertex-from-indexes (attrib-index world-coords ndc-coords normals tex-coords)
;;   (let ((coord-index (aref attrib-index 0))
;;         (tex-coord-index (aref attrib-index 1))
;;         (normal-index (aref attrib-index 2)))
;;     (make-vertex :coord (aref world-coords coord-index)
;;                  :ndc (aref ndc-coords coord-index)
;;                  :normal (aref normals normal-index)
;;                  :tex-coord (aref tex-coords tex-coord-index))))

;; (defun build-triangle-from-face (face world-coords ndc-coords normals tex-coords)
;;   (let ((v1 (build-vertex-from-indexes
;;              (aref face 0) world-coords ndc-coords normals tex-coords))
;;         (v2 (build-vertex-from-indexes
;;              (aref face 1) world-coords ndc-coords normals tex-coords))
;;         (v3 (build-vertex-from-indexes
;;              (aref face 2) world-coords ndc-coords normals tex-coords)))
;;     (build-triangle v1 v2 v3)))


;; out format:
;; vertices:
;; (#(v v v uv uv n n n) ...)
;; indices:
;; (#(i i i) ...)
(defun modelmesh-to-array (modelmesh &optional (with-tb nil))
  (flet ((hash-attr (v)
           (+ (ash (aref v 0) 64)
              (ash (aref v 1) 32)
              (aref v 2))))
    (let* ((vertex-array (make-array 0 :adjustable t :fill-pointer t))
           (vertex-index-ht (make-hash-table))
           (index-array (make-array (* 3 (length (modelmesh-faces modelmesh)))))
           (index 0))
      (doarray (face (modelmesh-faces modelmesh) face-i)
        (doarray (vertex face vertex-f-i)
          (let* ((vi (aref vertex 0))
                 (ti (aref vertex 1))
                 (ni (aref vertex 2))
                 (vertex-sig (hash-attr vertex)))
            (mvb-let* ((val foundp (gethash vertex-sig vertex-index-ht)))
              val
              (if (not foundp)
                  (let ((v (concatenate
                            '(vector single-float)
                            (vec3 (aref (modelmesh-vertices modelmesh) vi))
                            (aref (modelmesh-tex-coords modelmesh) ti)
                            (vec3 (aref (modelmesh-normals modelmesh) ni)))))
                    (vector-push-extend v vertex-array)
                    (setf (gethash vertex-sig vertex-index-ht) index)
                    (incf index))))
            (let ((vertex-i (gethash vertex-sig vertex-index-ht)))
              (setf (aref index-array (+ vertex-f-i (* 3 face-i))) vertex-i)))))
      ;; calculate Tangent and Bitangent
      ;; (if with-tb
      ;;     (doarray (face (modelmesh-faces modelmesh) face-i)
      ;;       (let* ((vi-a (mapvec (lambda (v) (aref v 0)) face))
      ;;              (ti-a (mapvec (lambda (v) (aref v 1)) face))
      ;;              (coords (mapvec (lambda (vi) (aref (modelmesh-vertices modelmesh) vi))
      ;;                              vi-a))
      ;;              (uvs (mapvec (lambda (ti) (aref (modelmesh-tex-coords modelmesh) ti))
      ;;                           ti-a))
      ;;              (e1 (vec3- (aref coords 1) (aref coords 0)))
      ;;              (e2 (vec3- (aref coords 2) (aref coords 1)))
      ;;              (uv1 (vec2- (aref uvs 1) (aref uvs 0)))
      ;;              (uv2 (vec2- (aref uvs 2) (aref uvs 1)))
      ;;              (a (/ 1.0 (let ((d (- (* (aref uv1 0)
      ;;                                       (aref uv2 1))
      ;;                                    (* (aref uv2 1))
      ;;                                    (aref uv1 0))))
      ;;                          (if (= d 0.0) 0.01 d))))
      ;;              (uvp (make-array '(2 2)
      ;;                               :initial-contents `((,(aref uv2 1)     ,(- (aref uv1 1)))
      ;;                                                   (,(- (aref uv2 0)) ,(aref uv1 0)))))
      ;;              (e (make-mat e1 e2))
      ;;              (tb-array (mat-mul-x a (mat-mul uvp e)))
      ;;              (tb (flat-mat tb-array))
      ;;              (vsigs (mapvec #'hash-attr face))
      ;;              (vertices (mapvec (lambda (vsig)
      ;;                                  (let ((v (aref vertex-array
      ;;                                                 (gethash vsig vertex-index-ht))))
      ;;                                    (concatenate '(vector single-float)
      ;;                                                 v tb)))
      ;;                                vsigs))
      ;;              (indicies (mapvec (lambda (vsig) (gethash vsig vertex-index-ht)) vsigs)))
      ;;        todo)))
      ;; flaten vertex array and return
      (let* ((vertex-length (if with-tb (+ 8 6) 8))
             (vertex-array-flat (make-array (* vertex-length (length vertex-array)))))
        (dotimes (vi (length vertex-array))
          (dotimes (i vertex-length)
            (setf (aref vertex-array-flat (+ i (* vertex-length vi)))
                  (aref (aref vertex-array vi) i))))
        (values vertex-array-flat index-array)))))
;; (mvb-let* ((vertices faces (modelmesh-to-array (wavefront-file-to-modelmesh "test.obj"))))
;;   (let ((vertex-num (/ (length vertices) 8)))
;;     (dotimes (index vertex-num)
;;       (format t "~A:" index)
;;       (dotimes (i 8)
;;         (format t "    ~A" (aref vertices (+ i (* index 8)))))
;;       (format t "~%"))
;;     (print faces)))
;; 0:    -0.5    0.5    0.0    0.0    1.0    0.0    0.0    1.0
;; 1:    0.5    0.5    0.0    1.0    1.0    0.0    0.0    1.0
;; 2:    0.5    -0.5    0.0    1.0    0.0    0.0    0.0    1.0
;; 3:    -0.5    0.5    0.0    0.0    1.0    0.0    0.0    -1.0
;; 4:    0.5    -0.5    0.0    1.0    0.0    0.0    0.0    -1.0
;; 5:    -0.5    -0.5    0.0    0.0    0.0    0.0    0.0    -1.0
;; #(0 1 2 3 4 5)
;; (mvb-let* ((vertices faces (modelmesh-to-array (wavefront-file-to-modelmesh "test1.obj"))))
;;   (let ((vertex-num (/ (length vertices) 8)))
;;     (dotimes (index vertex-num)
;;       (format t "~A:" index)
;;       (dotimes (i 8)
;;         (format t "    ~A" (aref vertices (+ i (* index 8)))))
;;       (format t "~%"))
;;     (print faces)))
;; 0:    -0.5    0.5    0.0    0.0    1.0    0.0    0.0    1.0
;; 1:    0.5    0.5    0.0    1.0    1.0    0.0    0.0    1.0
;; 2:    0.5    -0.5    0.0    1.0    0.0    0.0    0.0    1.0
;; 3:    -0.5    -0.5    0.0    0.0    0.0    0.0    0.0    1.0
;; #(0 1 2 0 2 3)


