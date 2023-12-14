;;;; controllers.lisp

(in-package #:sketch)

;;;   ____ ___  _   _ _____ ____   ___  _     _     _____ ____  ____
;;;  / ___/ _ \| \ | |_   _|  _ \ / _ \| |   | |   | ____|  _ \/ ___|
;;; | |  | | | |  \| | | | | |_) | | | | |   | |   |  _| | |_) \___ \
;;; | |__| |_| | |\  | | | |  _ <| |_| | |___| |___| |___|  _ < ___) |
;;;  \____\___/|_| \_| |_| |_| \_\\___/|_____|_____|_____|_| \_\____/

;;; Mouse

(defparameter *buttons* (list :left nil :middle nil :right nil))
(defparameter *current-entity* nil)

(defmethod on-click (instance x y))
(defmethod on-middle-click (instance x y))
(defmethod on-right-click (instance x y))
(defmethod on-click-release (instance x y))
(defmethod on-middle-click-release (instance x y))
(defmethod on-right-click-release (instance x y))
(defmethod on-hover (instance x y))
(defmethod on-enter (instance))
(defmethod on-leave (instance))

(defun propagate-to-entity (sketch x y f)
  (loop
    for entity being the hash-key of (sketch-%entities sketch)
    for (im iw ih) being the hash-value of (sketch-%entities sketch)
    for (ix iy) = (transform-vertex (list x y) im)
    when (and (< 0 ix iw) (< 0 iy ih))
      do (funcall f entity ix iy)))

(defmethod on-click :around ((*sketch* sketch) x y)
  (with-sketch (*sketch*)
    (let ((*draw-mode* nil))
      (propagate-to-entity *sketch* x y #'on-click)
      (call-next-method))))

(defmethod on-middle-click :around ((*sketch* sketch) x y)
  (with-sketch (*sketch*)
    (let ((*draw-mode* nil))
      (propagate-to-entity *sketch* x y #'on-middle-click)
      (call-next-method))))

(defmethod on-right-click :around ((*sketch* sketch) x y)
  (with-sketch (*sketch*)
    (let ((*draw-mode* nil))
      (propagate-to-entity *sketch* x y #'on-right-click)
      (call-next-method))))

(defmethod on-hover :around ((*sketch* sketch) x y)
  (with-sketch (*sketch*)
    (call-next-method)))

(defmethod on-hover :around ((entity entity) ix iy)
  (let ((*draw-mode* nil))
    (unless (eql *current-entity* entity)
      (on-leave *current-entity*)
      (setf *current-entity* entity)
      (on-enter entity))
    (call-next-method)))

(defmethod kit.sdl2:mousebutton-event ((instance sketch) state timestamp button x y)
  (let ((button (elt (list nil :left :middle :right) button))
        (click-method (elt (list nil #'on-click #'on-middle-click #'on-right-click) button))
        (release-method (elt (list nil #'on-click-release #'on-middle-click-release #'on-right-click-release) button)))
    (when (equal state :mousebuttondown)
      (setf (getf *buttons* button) t)
      (funcall click-method instance x y))
    (when (and (equal state :mousebuttonup) (getf *buttons* button))
      (setf (getf *buttons* button) nil)
      (funcall release-method instance x y))))

(defmethod kit.sdl2:mousemotion-event ((instance sketch) timestamp button-mask x y xrel yrel)
  (on-hover instance x y)
  (unless
      (loop for entity being the hash-key of (sketch-%entities instance)
            for (im iw ih) being the hash-value of (sketch-%entities instance)
            for (ix iy) = (transform-vertex (list x y) im)
            when (and (< 0 ix iw) (< 0 iy ih))
              do (on-hover entity ix iy)
                 (return t))
    (when *current-entity*
      (on-leave *current-entity*)
      (setf *current-entity* nil))))

(defmethod kit.sdl2:mousemotion-event :after ((instance sketch)
                                              timestamp button-mask x y xrel yrel)
  (out :mouse (cons x y)
       :mouse-x x
       :mouse-y y
       :mouse-rel (cons xrel yrel)
       :mouse-xrel xrel
       :mouse-yrel yrel))

(defmethod kit.sdl2:mousewheel-event :after ((instance sketch)
                                             timestamp x y)
  (out :mouse-wheel (cons x y)
       :mouse-wheel-x x
       :mouse-wheel-y y))

(defmethod kit.sdl2:mousebutton-event :after ((instance sketch)
                                              state timestamp button x y)
  (with-slots (%env) instance
    (when (env-red-screen %env)
      (setf (env-debug-key-pressed %env) t))))

;;; Keyboard

(defconstant +scancode-prefix-length+ 9)

(defmethod on-text (instance text))
(defmethod on-key (instance key state))

(defmethod on-text :around ((*sketch* sketch) text)
  (with-sketch (*sketch*)
    (call-next-method)))

(defmethod on-key :around ((*sketch* sketch) key state)
  (with-sketch (*sketch*)
    (call-next-method)))

(defmethod kit.sdl2:textinput-event :after ((instance sketch) timestamp text)
  (on-text instance text))

(defmethod kit.sdl2:keyboard-event :after ((instance sketch) state timestamp repeat-p keysym)
  (when (not repeat-p)
    (on-key instance
            ;; Removing the ugly "SCANCODE-" prefix from the keyword
            ;; symbol that denotes the button.
            (intern (subseq (symbol-name (sdl2:scancode keysym))
                            +scancode-prefix-length+)
                    (find-package "KEYWORD"))
            state)))

