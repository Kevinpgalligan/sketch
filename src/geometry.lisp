;;;; geometry.lisp

(in-package #:sketch)

;;;   ____ _____ ___  __  __ _____ _____ ______   __
;;;  / ___| ____/ _ \|  \/  | ____|_   _|  _ \ \ / /
;;; | |  _|  _|| | | | |\/| |  _|   | | | |_) \ V /
;;; | |_| | |__| |_| | |  | | |___  | | |  _ < | |
;;;  \____|_____\___/|_|  |_|_____| |_| |_| \_\|_|

(defun edges (vertices &optional (closed t))
  (loop
     for i in (if closed
                  (append (last vertices) (butlast vertices))
                  (butlast vertices))
     for j in (if closed
                  vertices
                  (cdr vertices))
     collect (list i j)))

(defmacro with-lines (lines &body body)
  (flet ((i-to-s (i) (format nil "~a" i)))
    `(symbol-macrolet
         ,(loop
             for line in lines
             for i upfrom 0 by 2
             append
               (loop
                  for sym in '(x x y y)
                  for idx in '(1 2 1 2)
                  for line-accessor in '(caar caadr cadar cadadr)
                  collect
                    `(,(alexandria:symbolicate sym (i-to-s (+ i idx)))
                       (,line-accessor ,line))))
       ,@body)))

(defun translate-line (line d)
  (with-lines (line)
    (let* ((a (atan (- y2 y1) (- x2 x1)))
           (dx (* (sin a) d))
           (dy (* (cos a) d)))
      `((,(+ x1 dx) ,(- y1 dy)) (,(+ x2 dx) ,(- y2 dy))))))

(defun intersect-lines (line1 line2)
  ;; https://en.wikipedia.org/wiki/Line–line_intersection#Given_two_points_on_each_line
  ;; The algorithm is changed so that division by zero never happens.
  ;; The values that are returned for "intersection" may or may not make sense, but
  ;; having responsive but wrong sketch is much better than a red screen.
  (with-lines (line1 line2)
    (let* ((denominator (- (* (- x1 x2) (- y3 y4))
                           (* (- y1 y2) (- x3 x4))))
           (a (if (zerop denominator)
                  (/ (+ x2 x3) 2)
                  (/ (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4))
                        (* (- (* x3 y4) (* y3 x4)) (- x1 x2)))
                     denominator)))
           (b (if (zerop denominator)
                  (/ (+ y2 y3) 2)
                  (/ (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4))
                        (* (- (* x3 y4) (* y3 x4)) (- y1 y2)))
                     denominator))))
      (list a b))))

(defun grow-polygon (polygon d)
  (let ((polygon
         (mapcar (lambda (x) (apply #'intersect-lines x))
                 (edges (mapcar (lambda (x) (translate-line x (- d)))
                                (edges polygon))))))
    (append (cdr polygon) (list (car polygon)))))

(defun triangulate (polygon)
  (let ((points (group polygon)))
    (apply #'append
           (glu-tessellate:tessellate
            (make-array (length points) :initial-contents points)
            :winding-rule (pen-winding-rule (env-pen *env*))))))

(defun bounding-box (vertices)
  (loop for (x y) in vertices
        minimize x into min-x
        maximize x into max-x
        minimize y into min-y
        maximize y into max-y
        finally (return (list (list min-x min-y) (list max-x max-y)))))

(defun normalize-to-bounding-box (vertices)
  (let ((box (bounding-box vertices)))
    (with-lines (box)
      (mapcar (lambda (vertex)
                (list (normalize (first vertex) x1 x2)
                      (normalize (second vertex) y1 y2)))
              vertices))))

(defun angle-between-lines (l1 l2)
  "Calculate angle between 2 lines in positive radians. Doesn't handle
lines of length 0. Lines are lists of 2 points; points are lists of 2 coordinates."
  (let ((v1 (line-as-vector l1))
        (v2 (line-as-vector l2)))
    (acos (/ (dot-product v1 v2)
             (vector-length v1)
             (vector-length v2)))))

(defun dot-product (v1 v2)
  (+ (* (first v1) (first v2))
     (* (second v1) (second v2))))

(defun interior-angle-between-lines (l1 l2)
  "Calculate interior angle between 2 lines, in positive radians."
  (angle-between-lines
   l1
   (if (< 0 (dot-product (line-as-vector l1) (line-as-vector l2)))
       l2
       (reverse-line l2))))

(defun reverse-line (line)
  (destructuring-bind ((x1 y1) (x2 y2)) line
    (let ((dx (- x2 x1))
          (dy (- y2 y1)))
      (list (first line)
            (list (- x1 dx) (- y1 dy))))))

(defun vector-length (v)
  (destructuring-bind (x y) v
    (sqrt (+ (* x x) (* y y)))))

(defun line-as-vector (line)
  "Takes a line (list of 2 points) and returns the vector connecting those points."
  (destructuring-bind ((x1 y1) (x2 y2)) line
    (list (- x2 x1) (- y2 y1))))
