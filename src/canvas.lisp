;;;; canvas.lisp

(in-package #:sketch)

;;;   ____    _    _   ___     ___    ____
;;;  / ___|  / \  | \ | \ \   / / \  / ___|
;;; | |     / _ \ |  \| |\ \ / / _ \ \___ \
;;; | |___ / ___ \| |\  | \ V / ___ \ ___) |
;;;  \____/_/   \_|_| \_|  \_/_/   \_|____/


(defclass canvas ()
  ((width :initarg :width :reader canvas-width)
   (height :initarg :height :reader canvas-height)
   (%image :initform nil :accessor %canvas-image)
   (%vector :initform nil :accessor %canvas-vector)
   (%locked :initform nil :accessor %canvas-locked)
   (%fbo :initform nil :accessor %canvas-fbo)
   (%rbo :initform nil :accessor %canvas-rbo)))

(defmacro with-fbo (&body body)
  (alexandria:with-gensyms (old-fbo)
    `(let ((,old-fbo (gl:get-integer :framebuffer-binding)))
       (unwind-protect
            (progn
              ,@body)
         (gl:bind-framebuffer :framebuffer ,old-fbo)))))

(defmacro with-drawing-to-canvas ((canvas) &body body)
  (alexandria:with-gensyms (fbo rbo y-axis old-y-axis)
    (alexandria:once-only (canvas)
      `(with-slots ((,fbo %fbo)
                    (,rbo %rbo))
           ,canvas
         (with-fbo
           ;; Create a framebuffer for drawing to if it doesn't
           ;; already exist.
           (when (null ,fbo)
             (setf ,fbo (gl:gen-framebuffer)
                   ,rbo (gl:gen-renderbuffer))
             (gl:bind-framebuffer :framebuffer ,fbo)
             (gl:bind-renderbuffer :renderbuffer ,rbo)
             (gl:renderbuffer-storage :renderbuffer
                                      :rgba32f
                                      (canvas-width ,canvas)
                                      (canvas-height ,canvas))
             (gl:framebuffer-renderbuffer :framebuffer
                                          :color-attachment0
                                          :renderbuffer
                                          ,rbo)
             (unless (= (cffi:foreign-enum-value '%gl:enum (gl:check-framebuffer-status :framebuffer))
                        (cffi:foreign-enum-value '%gl:enum :framebuffer-complete))
               (warn "Failed to create FBO for drawing to canvas.")
               (gl:delete-framebuffers (vector ,fbo))
               (gl:delete-renderbuffers (vector ,rbo))
               (setf ,fbo nil ,rbo nil))
             (gl:bind-framebuffer :framebuffer 0)
             (gl:bind-renderbuffer :renderbuffer 0))
           (let* ((,old-y-axis (sketch-y-axis *sketch*))
                  (,y-axis (if (eq ,old-y-axis :up) :down :up)))
             ;; If we were successful, bind the framebuffer so that
             ;; all the drawing operations target it.
             (when ,fbo
               (gl:bind-framebuffer :framebuffer ,fbo)
               ;; Draw upside-down because the output from read-pixels
               ;; is upside-down.
               (setf (sketch-y-axis *sketch*) ,y-axis)
               (maybe-change-viewport *sketch*)
               ;; First draw the canvas into the framebuffer so
               ;; that the drawing operations layer on top of it.
               (draw ,canvas))
             ;; Run the caller's drawing code.
             ,@body
             ;; Now read back the data from the FBO, copying it over to
             ;; the canvas.
             (when ,fbo
               (gl:bind-framebuffer :framebuffer ,fbo)
               (%gl:read-pixels 0 0
                                (canvas-width ,canvas) (canvas-height ,canvas)
                                :bgra
                                :unsigned-byte
                                (%canvas-vector-pointer ,canvas))
               (setf (sketch-y-axis *sketch*) ,old-y-axis)
               (maybe-change-viewport *sketch*))))))))

(defun canvas-get-pixel (canvas x y)
  "Fetches the pixel at coordinates (X, Y) from the canvas.
Returns 4 values: R, G, B and A, which are integers in the range 0-255."
  ;; Possible improvements:
  ;; 1. Deduplicate code, see CANVAS-PAINT and CANVAS-PAINT-RGBA255.
  ;; 2. If this is slow, could try switching to cffi:make-shareable-vector for
  ;;    storage. I think we could then access it like a normal array. As far
  ;;    as I remember, the CFFI interface is quite inefficient and does unnecessary
  ;;    cons-ing.
  (let ((base-index (* 4 (+ x (* (canvas-width canvas) y))))
        (ptr (%canvas-vector-pointer canvas)))
    (values
     (cffi:mem-aref ptr :uint8 (+ base-index 2))
     (cffi:mem-aref ptr :uint8 (+ base-index 1))
     (cffi:mem-aref ptr :uint8 base-index)
     (cffi:mem-aref ptr :uint8 (+ base-index 3)))))

(defun make-canvas (width height)
  (let ((canvas (make-instance 'canvas :width width :height height)))
    (canvas-reset canvas)
    canvas))

(defmethod %canvas-vector-pointer ((canvas canvas))
  (static-vectors:static-vector-pointer (%canvas-vector canvas)))

(defmethod canvas-reset ((canvas canvas))
  (setf (%canvas-vector canvas)
        (static-vectors:make-static-vector (* (canvas-width canvas) (canvas-height canvas) 4) :initial-element 0)))

(defmethod canvas-paint ((canvas canvas) (color color) x y)
  (let ((ptr (%canvas-vector-pointer canvas))
        (pos (+ (* x 4) (* y 4 (canvas-width canvas))))
        (vec (color-bgra-255 color)))
    (dotimes (i 4)
      (setf (cffi:mem-aref ptr :uint8 (+ pos i)) (elt vec i)))))

(defun canvas-paint-rgba255 (canvas x y r g b a)
  (let ((ptr (%canvas-vector-pointer canvas))
        (pos (+ (* x 4) (* y 4 (canvas-width canvas)))))
    (setf (cffi:mem-aref ptr :uint8 pos) b
          (cffi:mem-aref ptr :uint8 (+ pos 1)) g
          (cffi:mem-aref ptr :uint8 (+ pos 2)) r
          (cffi:mem-aref ptr :uint8 (+ pos 3)) a)))

(defun canvas-paint-gray255 (canvas x y amount)
  (canvas-paint-rgba255 canvas x y amount amount amount 255))

(defmethod canvas-image ((canvas canvas)
                         &key (min-filter :linear)
                              (mag-filter :linear)
                         &allow-other-keys)
  (if (%canvas-locked canvas)
      (%canvas-image canvas)
      (make-image-from-surface
       (sdl2:create-rgb-surface-with-format-from
        (%canvas-vector-pointer canvas)
        (canvas-width canvas)
        (canvas-height canvas)
        32
        (* 4 (canvas-width canvas))
        :format sdl2:+pixelformat-argb8888+)
       :min-filter min-filter
       :mag-filter mag-filter)))

(defmethod canvas-lock ((canvas canvas)
                        &key (min-filter :linear)
                             (mag-filter :linear)
                        &allow-other-keys)
  (setf (%canvas-image canvas) (canvas-image canvas
                                             :min-filter min-filter
                                             :mag-filter mag-filter)
        (%canvas-locked canvas) t))

(defmethod canvas-unlock ((canvas canvas))
  (setf (%canvas-locked canvas) nil))

(defmethod draw ((canvas canvas)
                 &key (x 0) (y 0) width height mode
                   (min-filter :linear)
                   (mag-filter :linear)
                 &allow-other-keys)
  "Draws a canvas with its top-left corner at co-ordinates X & Y. By default,
uses the width and height that the canvas was created with, but these can be
overwritten by parameters WIDTH and HEIGHT.

MIN-FILTER and MAG-FILTER are used to determine pixel colours when the
drawing area is smaller or larger, respectively, than the canvas. By default,
the :LINEAR function is used. :NEAREST is also a common option. Note that, if
CANVAS-LOCK is being used, then MIN-FILTER and MAG-FILTER should be passed
there instead.

See: https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexParameter.xhtml"
  (declare (ignore mode))
  (draw (canvas-image canvas :min-filter min-filter :mag-filter mag-filter)
        :x x
        :y y
        :width (or width (canvas-width canvas))
        :height (or height (canvas-height canvas))))
