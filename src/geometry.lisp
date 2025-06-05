;;;; geometry.lisp

(in-package #:sketch)

;;;   ____ _____ ___  __  __ _____ _____ ______   __
;;;  / ___| ____/ _ \|  \/  | ____|_   _|  _ \ \ / /
;;; | |  _|  _|| | | | |\/| |  _|   | | | |_) \ V /
;;; | |_| | |__| |_| | |  | | |___  | | |  _ < | |
;;;  \____|_____\___/|_|  |_|_____| |_| |_| \_\|_|

(defun make-point (x y)
  (list x y))
(defun point-x (p) (first p))
(defun point-y (p) (second p))
(defun point-subtract (p1 p2)
  (make-point (- (point-x p1) (point-x p2))
              (- (point-y p1) (point-y p2))))

(defun edges (vertices &optional (closed t))
  (loop
     for i in (if closed
                  (append (last vertices) (butlast vertices))
                  (butlast vertices))
     for j in (if closed
                  vertices
                  (cdr vertices))
     collect (list i j)))

(defun make-line (point1 point2)
  (list point1 point2))

(defun line-start (line)
  (first line))

(defun line-end (line)
  (second line))

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
        finally (return (make-bounding-box min-x min-y max-x max-y))))

(defun make-bounding-box (min-x min-y max-x max-y)
  (list (list min-x min-y) (list max-x max-y)))

(defun bounding-box-min-x (bb)
  (first (first bb)))

(defun bounding-box-min-y (bb)
  (second (first bb)))

(defun bounding-box-max-x (bb)
  (first (second bb)))

(defun bounding-box-max-y (bb)
  (second (second bb)))

(defun normalize-to-bounding-box (box x y)
  (with-lines (box)
    (values (normalize x x1 x2)
            (normalize y y1 y2))))

(defun intersect-bounding-boxes (bb1 bb2)
  (and (range-intersects? (bounding-box-min-x bb1) (bounding-box-max-x bb1)
                          (bounding-box-min-x bb2) (bounding-box-max-x bb2))
       (range-intersects? (bounding-box-min-y bb1) (bounding-box-max-y bb1)
                          (bounding-box-min-y bb2) (bounding-box-max-y bb2))
       (make-bounding-box (max (bounding-box-min-x bb1) (bounding-box-min-x bb1))
                          (max (bounding-box-min-y bb1) (bounding-box-min-y bb1))
                          (min (bounding-box-max-x bb1) (bounding-box-max-x bb1))
                          (min (bounding-box-max-y bb1) (bounding-box-max-y bb1)))))

(defun range-intersects? (r1-lo r1-hi r2-lo r2-hi)
  (not (or (< r1-hi r2-lo)
           (< r2-hi r1-lo))))

(defun bounding-box-contains? (bb point)
  (and (<= (bounding-box-min-x bb)
           (point-x point)
           (bounding-box-max-x bb))
       (<= (bounding-box-min-y bb)
           (point-y point)
           (bounding-box-max-y bb))))

(defun line-segments-intersect? (l1 l2)
  (let ((bb1 (bounding-box l1))
        (bb2 (bounding-box l2)))
    (let ((bb-intersection (intersect-bounding-boxes bb1 bb2)))
      (and bb-intersection
           (bounding-box-contains? bb-intersection (intersect-lines l1 l2))))))

(defun calc-interior-angle (v1 v2)
  "Calculates interior angle between two vectors, in positive radians.
Result should be between 0 and pi."
  (acos (max -1
             (min 1
                  (/ (dot-product v1 v2)
                     (vector-length v1)
                     (vector-length v2))))))

(defun calc-line-segments-interior-angle (l1 l2)
  "Takes two line segments L1 and L2 attached end-to-end, and
calculates the interior angle between them."
  (let* ((intersect (intersect-lines l1 l2))
         (v1 (point-subtract (line-start l1) intersect))
         (v2 (point-subtract (line-end l2) intersect)))
    (calc-interior-angle v1 v2)))

(defun dot-product (v1 v2)
  (+ (* (first v1) (first v2))
     (* (second v1) (second v2))))

(defun vector-length (v)
  (destructuring-bind (x y) v
    (sqrt (+ (* x x) (* y y)))))

(defun line-as-vector (line)
  "Takes a line (list of 2 points) and returns the vector connecting those points."
  (destructuring-bind ((x1 y1) (x2 y2)) line
    (list (- x2 x1) (- y2 y1))))
