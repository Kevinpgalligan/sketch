;;;; utils.lisp

(in-package #:sketch)

;;;  _   _ _____ ___ _     ____
;;; | | | |_   _|_ _| |   / ___|
;;; | | | | | |  | || |   \___ \
;;; | |_| | | |  | || |___ ___) |
;;;  \___/  |_| |___|_____|____/

(defun pad-list (list pad length)
  (if (>= (length list) length)
      list
      (append (make-list (- length (length list)) :initial-element pad)
	      list)))

(defun group-bits (x &key (bits 8))  
  (let ((bit-fill (1- (expt 2 bits))))
    (do* ((x x (ash x (- bits)))
	  (acc `(,(boole boole-and x bit-fill))
	       (cons (boole boole-and x bit-fill) acc)))
	 ((zerop x) (cdr acc)))))
