(defun make-dynamic-string ()
  (make-array 0 :adjustable t
              :fill-pointer 0
              :element-type 'character))
;; (let ((s (make-dynamic-string)))
;;   (vector-push-extend #\a s)
;;   (vector-push-extend #\b s)
;;   s)

(defmacro mvb-let* (bindings &body body)
  (let* ((exp (car bindings))
         (vars (butlast exp))
         (multi-val-exp (car (last exp)))
         (rest-bindings (cdr bindings)))
    (if rest-bindings
        `(multiple-value-bind ,vars ,multi-val-exp
           (mvb-let* ,rest-bindings ,@body))
        `(multiple-value-bind ,vars ,multi-val-exp
           ,@body))))
;; (defun test-binding-0 ()
;;   (values 1 2))
;; (defun test-binding-1 ()
;;   (values 1 2 3))
;; (mvb-let* ((a b (test-binding-0))
;;            (c d e (test-binding-1)))
;;   (list a b c d e))

(defmacro doarray ((e array &optional (index nil)) &body body)
  (let ((index (if index index (gensym))))
    `(dotimes (,index (length ,array))
       (let ((,e (aref ,array ,index)))
         ,@body))))
;; (doarray (e #(0 2 4))
;;   (format t "~A  " e))
;; > 0  2  4
;; (doarray (e #(0 2 4) i)
;;   (format t "~A:~A  " i e))
;; > 0:0  1:2  2:4

(defun mapvec (function vector &optional (type t))
  (let* ((new-array (make-array (length vector) :element-type type)))
    (doarray (element vector index)
      (setf (aref new-array index) (funcall function element)))
    new-array))
;; (mapvec (lambda (x) (float (1+ x))) #(1 2 3 4) :single-float)

(defun parse-float (string)
  (flet ((report-error () (error (format nil "malformed float: \"~A\"" string))))
    (let ((state :s)
          (negative nil)
          (int 0)
          (dec 0.0)
          (dec-coef 0.1)
          (index 0))
      (loop
         (case state
           (:s (progn
                 (if (= index (length string)) (report-error))
                 (let ((char (aref string index)))
                   (cond ((char= char #\.) (setf state :dec))
                         ((char= char #\-) (progn (setf state :neg)
                                                  (setf negative t)))
                         ((<= (char-code #\0) (char-code char) (char-code #\9))
                          (progn (setf state :int)
                                 (setf int (- (char-code char) (char-code #\0)))))
                         (t (report-error))))
                 (incf index)))
           (:neg (progn
                   (if (= index (length string)) (report-error))
                   (let ((char (aref string index)))
                     (cond ((<= (char-code #\0) (char-code char) (char-code #\9))
                            (progn (setf state :int)
                                   (setf int (- (char-code char) (char-code #\0)))))
                           ((char=  char #\.)
                            (setf state :dec))
                           (t (report-error))))
                   (incf index)))
           (:int (progn
                   (if (= index (length string)) (return))
                   (let ((char (aref string index)))
                     (cond ((<= (char-code #\0) (char-code char) (char-code #\9))
                            (setf int (+ (- (char-code char) (char-code #\0))
                                         (* int 10))))
                           ((char= char #\.) (setf state :dec))
                           (t (report-error))))
                   (incf index)))
           (:dec (progn
                   (if (= index (length string)) (report-error))
                   (let ((char (aref string index)))
                     (cond ((<= (char-code #\0) (char-code char) (char-code #\9))
                            (progn (incf dec (* dec-coef (- (char-code char) (char-code #\0))))
                                   (setf dec-coef (* 0.1 dec-coef))
                                   (setf state :dec1)))
                           (t (report-error))))
                   (incf index)))
           (:dec1 (progn
                   (if (= index (length string)) (return))
                   (let ((char (aref string index)))
                     (cond ((<= (char-code #\0) (char-code char) (char-code #\9))
                            (progn (incf dec (* dec-coef (- (char-code char) (char-code #\0))))
                                   (setf dec-coef (* 0.1 dec-coef))))
                           (t (report-error))))
                   (incf index)))))
      (* (if negative -1 1) (+ int dec)))))

;; (mapcar #'parse-float '("1234567" "3" "-3" "0" "-0" "-0.3" "0.3" "3.33" "-33.33" ".33" "0.321"))
;; (parse-float "323.232374")
;; (dolist (s (mapcar (lambda (string)
;;                      (handler-case (parse-float string)
;;                        (condition (e) (format nil "~e" e))))
;;                    '("" "."
;;                      "123." "-")))
;;   (format t "~A~%" s))

(defun read-file-as-string (pathname)
  (with-open-file (s pathname)
    (with-output-to-string (out)
      (do ((line (read-line s nil nil) (read-line s nil nil)))
          ((null line) nil)
        (write-line line out))
      out)))


