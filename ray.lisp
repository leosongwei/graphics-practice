;;;; line and triangle intersection
;;;; see:
;;;; Doug Baldwin and Michael Weber, Fast Ray-Triangle Intersections by Coordinate Transformation, Journal of Computer Graphics Techniques (JCGT), vol. 5, no. 3, 39-49, 2016
;;;; http://jcgt.org/published/0005/03/03/

(defun calculate-project-mat (v1 v2 v3)
  (let* ((e1 (vec3- v2 v1))
         (e2 (vec3- v3 v1))
         (n (vec3-cross e1 e2))
         (trans-inv
          (let ((n0 (aref n 0)) (n1 (aref n 1)) (n2 (aref n 2))
                (e1-0 (aref e1 0)) (e1-1 (aref e1 1)) (e1-2 (aref e1 2))
                (e2-0 (aref e2 0)) (e2-1 (aref e2 1)) (e2-2 (aref e2 2))
                (v3-x-v1 (vec3-cross v3 v1))
                (v2-x-v1 (vec3-cross v2 v1))
                (n.v1 (vec3-dot n v1)))
            (case (max-is-nth n)
              (0
               (make-array
                '(4 4) :element-type 'single-float
                :initial-contents
                `((0.0 ,(/ e2-2 n0) ,(- (/ e2-1 n0)) ,(/ (aref v3-x-v1 0) n0))
                  (0.0 ,(- (/ e1-2 n0)) ,(/ e1-1 n0) ,(- (/ (aref v2-x-v1 0) n0)))
                  (1.0 ,(/ n1 n0) ,(/ n2 n0) ,(- (/ n.v1 n0)))
                  (0.0 0.0 0.0 1.0))))
              (1
               (make-array
                '(4 4) :element-type 'single-float
                :initial-contents
                `((,(- (/ e2-2 n1)) 0.0 ,(/ e2-0 n1) ,(/ (aref v3-x-v1) 1) n1)
                  (,(/ e1-2 n1) 0.0 ,(- (/ e1-0 n1)) ,(- (/ (aref v2-x-v1 1) n1)))
                  (,(/ n0 n1) 1.0 ,(/ n2 n1) ,(- (/ n.v1 n1)))
                  (0.0 0.0 0.0 1.0))))
              (2
               (make-array
                '(4 4) :element-type 'single-float
                :initial-contents
                `((,(/ e2-1 n2) ,(- (/ e2-0 n2)) 0.0 ,(/ (aref v3-x-v1 2) n2))
                  (,(- (/ e1-1 n2)) ,(/ e1-0 n2) 0.0 ,(- (/ (aref v2-x-v1 2) n2)))
                  (,(/ n0 n2) ,(/ n1 n2) 1.0 ,(- (/ n.v1 n2)))
                  (0.0 0.0 0.0 1.0))))))))
    trans-inv))
;; (calculate-project-mat (make-vec3 0.0 1.0 -2.0)
;;                        (make-vec3 -1.0 -1.0 -2.0)
;;                        (make-vec3 1.0 -1.0 -2.0))
;; #2A((-0.5 -0.25 0.0 0.25)
;;     (0.5 -0.25 0.0 0.25)
;;     (0.0 0.0 1.0 2.0)
;;     (0.0 0.0 0.0 1.0))
