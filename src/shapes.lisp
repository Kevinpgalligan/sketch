;;;; shapes.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____
;;; / ___|| | | |  / \  |  _ \| ____/ ___|
;;; \___ \| |_| | / _ \ | |_) |  _| \___ \
;;;  ___) |  _  |/ ___ \|  __/| |___ ___) |
;;; |____/|_| |_/_/   \_\_|   |_____|____/


(defparameter *miter-join-min-angle* (radians 15)
  "The minimum angle below which miter join starts to show visual artifacts
or look ugly, and should be swapped for another join algorithm.")

(defun point (x y)
  (declare (type real x y))
  (let ((weight (or (pen-weight (env-pen *env*)) 1)))
    (with-pen (make-pen :fill (pen-stroke (env-pen *env*)))
      (circle x y (/ weight 2)))))

(defun make-line-func (x1 y1 x2 y2)
  (let* ((a (atan (- y2 y1) (- x2 x1)))
         (w (/ (or (pen-weight (env-pen *env*)) 1) 2))
         (dx (* (sin a) w))
         (dy (* (cos a) w)))
    (lambda ()
      (draw-shape
       :triangle-strip
       `((,(- x1 dx) ,(+ y1 dy))
         (,(- x2 dx) ,(+ y2 dy))
         (,(+ x1 dx) ,(- y1 dy))
         (,(+ x2 dx) ,(- y2 dy)))
       nil))))

(defun line (x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (with-pen (flip-pen (env-pen *env*))
    (funcall (make-line-func x1 y1 x2 y2))))

(defun make-polyline-func (&rest coordinates)
  (multiple-value-bind (d+ d-)
      (div2-inexact (pen-weight (env-pen *env*)))
    (let ((lines (edges (group coordinates) nil)))
      (let ((triangle-strips (join-lines lines d+ (- d-))))
        (lambda ()
          (loop for strip in triangle-strips
                do (draw-shape
                    :triangle-strip
                    (mix-strip-vertices strip)
                    nil)))))))

(defclass triangle-strip ()
  ((left-vertices
    :initarg :left-vertices
    :accessor left-vertices)
   (right-vertices
    :initarg :right-vertices
    :accessor right-vertices)))

(defun mix-strip-vertices (triangle-strip)
  (mix-lists (left-vertices triangle-strip)
             (right-vertices triangle-strip)))

(defun join-lines (lines d+ d-)
  "Returns a list of triangle strips that, together, make up the polyline."
  (let* (strips
         (first-line (first lines))
         ;; The first point of the first line gives the first point
         ;; of the polyline.
         (lefts (list (line-start (translate-line first-line d+))))
         (rights (list (line-start (translate-line first-line d-)))))
    (flet ((make-strip ()
             (make-instance 'triangle-strip
                            :left-vertices (reverse lefts)
                            :right-vertices (reverse rights))))
      (loop for (l1 . (l2 . rest)) on lines
            while l1
            do (if (null l2)
                   ;; The last point of the last line gives the end
                   ;; of the polyline.
                   (progn
                     (push (line-end (translate-line l1 d+)) lefts)
                     (push (line-end (translate-line l1 d-)) rights))
                   ;; If not the last line, need to join these 2 lines using some
                   ;; join algorithm.
                   ;; See, for example:
                   ;;   https://mattdesl.svbtle.com/drawing-lines-is-hard
                   ;;   http://bluevoid.com/opengl/sig00/advanced00/notes/node290.html
                   (let ((l1-left (translate-line l1 d+))
                         (l1-right (translate-line l1 d-))
                         (l2-left (translate-line l2 d+))
                         (l2-right (translate-line l2 d-))
                         (angle (calc-line-segments-interior-angle l1 l2)))
                     (multiple-value-bind (new-lefts new-rights new-strip?)
                         (funcall (get-join-func angle)
                                  l1 l2 l1-left l1-right l2-left l2-right angle)
                       (map nil (lambda (p) (push p lefts)) new-lefts)
                       (map nil (lambda (p) (push p rights)) new-rights)
                       ;; The join function can indicate that we should
                       ;; start a new triangle strip. In this case, we have
                       ;; to treat the left vertices it returns as the new
                       ;; right vertices, and vice versa. This is specifically
                       ;; to make sure that the vertices returned by bevel join
                       ;; will form a triangle strip that fills the polyline.
                       ;; (See BEVEL-JOIN).
                       (when new-strip?
                         (push (make-strip) strips)
                         (setf lefts new-rights)
                         (setf rights new-lefts))))))
      (cons (make-strip) strips))))

(defun get-join-func (angle)
  (let ((join-type (pen-line-join (env-pen *env*))))
    (case join-type
      (:dynamic
       (if (< angle *miter-join-min-angle*)
           #'bevel-join
           #'miter-join))
      (:miter #'miter-join)
      (:bevel #'bevel-join)
      (t (error (format nil "Unknown join type '~a'" join-type))))))

(defun miter-join (l1 l2 l1-left l1-right l2-left l2-right angle)
  (declare (ignore l1 l2 angle))
  (values (list (intersect-lines l1-left l2-left))
          (list (intersect-lines l1-right l2-right))))

(defparameter *bevel-fallback-angle* (radians 20))

(defun bevel-join (l1 l2 l1-left l1-right l2-left l2-right angle)
  (let ((v1 (line-as-vector l1))
        (v2 (line-as-vector l2))
        (maybe-use-fallback? (< angle *bevel-fallback-angle*)))
    ;; First, figure out whether it's a right or left turn.
    ;; To do this: convert to coordinate system where v1 points along
    ;; the positive x-axis. Then, if v2 is above the x-axis, we're at a
    ;; left turn, and the left side of the line is interior.
    (if (< 0 (+ (* (second v1) (first v2))
                (* (- (first v1)) (second v2))))
        ;; Left turn, so bevel the right side of the polyline and
        ;; extend the left edges until they intersect.
        ;; Unless... for sharp angles (as arbitrarily determined by
        ;; *BEVEL-FALLBACK-ANGLE*) and thick polylines, sometimes the
        ;; intersection falls outside the thickened line segments and
        ;; this results in the triangle strip going outside the polyline.
        ;; If we detect this to be the case, we resort to a fallback:
        ;; end the current triangle strip (indicated by the third return
        ;; value being T) and begin a new one. The end points of L1
        ;; are used as the new start points, except the left & right
        ;; point should be swapped so that the polyline is filled properly
        ;; by the new triangle strip. (Draw it to understand why).
        (if (or (not maybe-use-fallback?)
                (line-segments-intersect? l1-left l2-left))
            (values (duplicate-list (list (intersect-lines l1-left l2-left)))
                    (list (line-end l1-right) (line-start l2-right))
                    nil)
            (values (list (line-end l1-left))
                    (list (line-end l1-right))
                    t))
        ;; Right turn, so do the opposite.
        (if (or (not maybe-use-fallback?)
                (line-segments-intersect? l1-right l2-right))
            (values (list (line-end l1-left) (line-start l2-left))
                    (duplicate-list
                     (list (intersect-lines l1-right l2-right)))
                    nil)
            (values (list (line-end l1-left))
                    (list (line-end l1-right))
                    t)))))

(defun polyline (&rest coordinates)
  (case (pen-weight (env-pen *env*))
    (nil nil)
    (1 (mapcar (lambda (x) (line (caar x) (cadar x) (caadr x) (cadadr x)))
               (edges (group coordinates) nil)))
    (t (with-pen (flip-pen (env-pen *env*))
         (funcall (apply #'make-polyline-func coordinates))))))

(defun make-rect-func (x y w h)
  (if (and (plusp w) (plusp h))
      (lambda ()
        (draw-shape
         :triangle-strip
         `((,x ,(+ y h)) (,x ,y) (,(+ x w) ,(+ y h)) (,(+ x w) ,y))
         `((,x ,y) (,x ,(+ y h)) (,(+ x w) ,(+ y h)) (,(+ x w) ,y))))
      (lambda ())))

(defun rect (x y w h)
  (declare (type real x y w h))
  (funcall (make-rect-func x y w h)))

(defun ngon-vertices (n cx cy rx ry &optional (angle 0))
  (let* ((angle (radians angle))
         (rx (if (zerop rx) +epsilon+ rx))
         (theta (/ +tau+ n))
         (tangential (tan theta))
         (radial (cos theta))
         (y-mul (/ ry rx)))
    (loop repeat n
          for x = (* (cos angle) rx) then (* radial (- x (* (- y) tangential)))
          and y = (* (sin angle) ry) then (* radial (- y (* x tangential)))
          collect `(,(+ x cx) ,(+ (* y-mul y) cy)))))

(defun make-ngon-func (n cx cy rx ry &optional (angle 0))
  (let ((vertices (ngon-vertices n cx cy rx ry angle)))
    (lambda ()
      (draw-shape :triangle-fan vertices vertices))))

(defun ngon (n cx cy rx ry &optional (angle 0))
  (declare (type fixnum n)
           (type real cx cy rx ry angle))
  (funcall (make-ngon-func n cx cy rx ry angle)))

(defun make-star-func (n cx cy ra rb &optional (angle 0))
  (let ((vertices (mix-lists (ngon-vertices n cx cy ra ra (+ 90 angle))
                             (ngon-vertices n cx cy rb rb (- (+ 90 angle) (/ 180 n))))))
    (lambda ()
      (draw-shape :triangle-fan
                  (list* (list cx cy)
                         (car (last vertices))
                         vertices)
                  vertices))))

(defun star (n cx cy ra rb &optional (angle 0))
  (declare (type fixnum n)
           (type real cx cy ra rb angle))
  (funcall (make-star-func n cx cy ra rb angle)))

(defun ellipse (cx cy rx ry)
  (declare (type real cx cy rx ry))
  (when (and (not (zerop rx)) (not (zerop ry)))
    (ngon (max 24 (truncate (* 5 (sqrt (/ (+ (abs rx) (abs ry)) 2)))))
          cx cy (abs rx) (abs ry))))

(defun circle (x y r)
  (declare (type real x y r))
  (when (not (zerop r))
    (ellipse x y (abs r) (abs r))))

(defun make-polygon (&rest coordinates)
  (list
   :triangles
   (triangulate coordinates)
   (group coordinates)))

(defun polygon (&rest coordinates)
  (apply #'draw-shape (apply #'make-polygon coordinates)))

(defun quadratic-bezier-point (v a b c)
  (let* ((d (lerp-lists v a b))
         (e (lerp-lists v b c)))
    (lerp-lists v d e)))

(defun cubic-bezier-point (v a b c d)
  (let* ((e (lerp-lists v a b))
         (f (lerp-lists v b c))
         (g (lerp-lists v c d)))
    (quadratic-bezier-point v e f g)))

(defun bezier (x1 y1 bx1 by1 bx2 by2 x2 y2)
  (declare (type real x1 y1 bx1 by1 bx2 by2 x2 y2))
  (let ((a (list x1 y1))
        (b (list bx1 by1))
        (c (list bx2 by2))
        (d (list x2 y2))
        (cs (max 2 (pen-curve-steps (env-pen *env*)))))
    (apply #'polyline
           (mapcan (lambda (v) (cubic-bezier-point v a b c d))
                   (alexandria:iota (1+ cs) :step (/ 1 cs))))))
