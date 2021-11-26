;;; demap.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://github.com/sawyer>
;; Maintainer: Sawyer Gardner <sawyerjgardner@gmail.com>
;; Created: November 25, 2021
;; Modified: November 25, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/sawyer/demap
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;--dependentys
(eval-when-compile
  (require 'cl-lib) )


;;minimap object
(defun demap-minimap-p(minimap)
  "Determin if MINIMAP is a demap-minimap."
  (and (listp minimap)
       (eq (car minimap) 'demap-minimap) ))

(defun demap-minimap-buffer(minimap)
  "Return the buffer used by MINIMAP.
this buffer can get killed when the minimap
switches what buffer it is shadowing."
  (nth 1 minimap))
(gv-define-setter demap-minimap-buffer(val minimap)
  `(setf (nth 1 ,minimap) ,val))

(defun demap-minimap-live-p(minimap)
  "Determin if MINIMAP is live."
  (and (demap-minimap-p minimap)
       (buffer-live-p (demap-minimap-buffer minimap)) ))


(defun demap-generate-minimap(name)
  "Genorate and return a new minimap with name NAME."
  (let ((new-map `(demap-minimap ,nil)))
    (setf (demap-minimap-buffer new-map) (generate-new-buffer name))
    new-map ))

(defun demap-kill-minimap(minimap)
  "Distroy the minimap buffer MINIMAP."
  (cl-assert (demap-minimap-p minimap) t "Wrong type argument: demap-minimap, %s")
  (when (demap-minimap-live-p minimap)
    (kill-buffer (demap-minimap-buffer minimap)) ))


;;debug
(defvar demap-minimaps '()
  "Debug list of minimaps.")


(defun demap-minimaps-new(name)
  "Make and return a new minimap with name NAME."
  (push (demap-generate-minimap name) demap-minimaps) )

(defun demap-minimaps-clear()
  "."
  (dolist (map demap-minimaps)
    (demap-kill-minimap map) )
  (setq demap-minimaps '()) )


(provide 'demap)
;;; demap.el ends here
