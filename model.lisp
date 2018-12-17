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
  vertices  ;; #(float-vec4 ...)
  tex-coords ;; #(float-vec4 ...)
  normals ;; #(float-vec4 ...)
  faces) ;; #(face),
;; face:#(int-vec3 int-vec3 int-vec3)
;;        coords   uvs      normals
;; vertex attribute index, 0-indexed, not like wavefront!!

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
(defun modelmesh-to-array (modelmesh)
  (flet ((hash-attr (v)
           (let ((vi (aref v 0)) (ui (aref v 1)) (ni (aref v 2)))
             (+ (ash vi 64) (ash ui 32) ni))))
    (let ((vertex-ht (make-hash-table))
          (vertex-i-ht (make-hash-table))
          (vertex-list nil) ;; (#(v v v uv uv n n n) ...) 8
          (face-list nil))  ;; (#(i i i) ...)
      ;; compact vertices
      (doarray (f (modelmesh-faces modelmesh))
        (doarray (indices f)
          (let* (;; index
                 (coord-i (aref indices 0))
                 (uv-i (aref indices 1))
                 (normal-i (aref indices 2))
                 ;; vectors
                 (coord (vec3 (aref (modelmesh-vertices modelmesh) coord-i)))
                 (uv (aref (modelmesh-tex-coords modelmesh) uv-i))
                 (normal (vec3 (aref (modelmesh-normals modelmesh) normal-i))))
            (setf (gethash (hash-attr indices) vertex-ht)
                  (concatenate '(vector single-float) coord uv normal)))))
      ;; assign output vertex index
      ;; and build vertex list
      (let ((index 0))
        (with-hash-table-iterator (next-vertex vertex-ht)
          (loop (mvb-let* ((more? key value (next-vertex)))
                  (if (null more?) (return))
                  (setf (gethash key vertex-i-ht) index)
                  (push value vertex-list)
                  (incf index)))))
      (setf vertex-list (reverse vertex-list))
      ;; build faces list
      (doarray (f (modelmesh-faces modelmesh))
        (doarray (vis f)
          (let ((index (gethash (hash-attr vis) vertex-i-ht)))
            (push index face-list))))
      (setf face-list (reverse face-list))
      ;; flaten vertex list and face list then return
      (let ((vertex-array (make-array (* 8 (length vertex-list))
                                      :element-type 'single-float))
            (face-array (make-array (length face-list)
                                    :element-type 'integer)))
        ;; vertex
        (let ((index 0))
          (dolist (v vertex-list)
            (dotimes (i 8)
              (setf (aref vertex-array (+ i (* index 8))) (aref v i)))
            (incf index)))
        ;; face
        (let ((index 0))
          (dolist (f face-list)
            (setf (aref face-array index) f)
            (incf index)))
        (values vertex-array face-array)))))

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
