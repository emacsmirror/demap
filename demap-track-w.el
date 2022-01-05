;;; demap-track-w.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://github.com/sawyer>
;; Maintainer: Sawyer Gardner <sawyerjgardner@gmail.com>
;; Created: January 04, 2022
;; Modified: January 04, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/sawyer/demap-track-w
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (when (>= emacs-major-version 28)
    ;window.el doesn't provide 'window before version 28
    (require 'window) ))

(require 'demap--tools)
(require 'demap-minimap)

(defface demap-visible-region-face
  '((((background dark)) (:background "gray10" :extend t))
    (t (:background "gray10" :extend t)))
  "Face used to for the part of the minimap visible throw the main window."
  :group 'demap)

(defface demap-current-line-face
  '((((background dark)) (:background "gray50" :extend t))
    (t (:background "gray50" :extend t)))
  "Face used to show the current line."
  :group 'demap)

(defface demap-visible-region-inactive-face
  '((((background dark)) (:background "gray16" :extend t))
    (t (:background "gray16" :extend t)))
  "Face used to represent the part of the minimap visible throw the main window."
  :group 'demap)

(defface demap-current-line-inactive-face
  '((((background dark)) (:background "gray77" :extend t))
    (t (:background "gray77" :extend t)))
  "Face used to show the current line."
  :group 'demap)


;;;line-ov-------

(cl-defstruct (demap-line-ov
               (:copier nil)
               (:constructor nil)
               (:constructor demap-line-ov-construct
                (minimap
                 &aux
                 (overlay (make-overlay 0 0 (demap-minimap-buffer minimap))) )))
  (minimap nil :read-only t)
  (overlay nil :read-only t)
  (window  nil) )


;;line-ov update

(defun demap-line-ov-update(line-ov)
  ""
  (if (eq (selected-window) (demap-line-ov-window line-ov))
      (move-overlay (demap-line-ov-overlay line-ov)
                    (line-beginning-position)
                    (+ (line-end-position) 1)
                    (demap-minimap-buffer (demap-line-ov-minimap line-ov)) )
    (demap-line-ov-unset line-ov) ))

;;line-ov set

(defun demap-line-ov-unset(line-ov)
  ""
  (overlay-put (demap-line-ov-overlay line-ov) 'face 'demap-current-line-inactive-face)
  (remove-hook 'post-command-hook (apply-partially #'demap-line-ov-update line-ov)) )

(defun demap-line-ov-set(line-ov window)
  ""
  (when window
    (overlay-put (demap-line-ov-overlay line-ov) 'face 'demap-current-line-face)
    (setf (demap-line-ov-window line-ov) window)
    (add-hook 'post-command-hook (apply-partially #'demap-line-ov-update line-ov))
    (demap-line-ov-update line-ov) ))


;;;area-ov-------

(cl-defstruct (demap-area-ov
               (:copier nil)
               (:constructor nil)
               (:constructor demap-area-ov-construct
                (minimap
                 &aux
                 (overlay (make-overlay 0 0 (demap-minimap-buffer minimap))) )))
  (minimap nil :read-only t)
  (overlay nil :read-only t)
  (window  nil) )


(defun demap-area-ov-active-p(area-ov)
  ""
  (let ((window  (demap-area-ov-window  area-ov))
        (minimap (demap-area-ov-minimap area-ov)) )
    (and (window-live-p window)
         (eq (demap--tools-real-buffer (window-buffer window)) (demap-minimap-showing minimap)) )))

;;area-ov update

(defun demap-area-ov-update(area-ov &optional &rest r)
  ""
  (ignore r)
  (let ((window (demap-area-ov-window area-ov)))
    (if (demap-area-ov-active-p area-ov)
        (move-overlay (demap-area-ov-overlay area-ov)
                      (window-start window)
                      (window-end window t)
                      (demap-minimap-buffer (demap-area-ov-minimap area-ov)) )
      (demap-area-ov-unset area-ov) )))

(defun demap-area-ov-update-if-window(area-ov window &optional &rest r)
  ""
  (ignore r)
  (when (eq (demap-area-ov-window area-ov) window)
    (demap-area-ov-update area-ov) ))

;;area-ov set

(defun demap-area-ov-unset(area-ov)
  ""
  (let ((upd-if-finc (apply-partially #'demap-area-ov-update-if-window area-ov))
        (update-func (apply-partially #'demap-area-ov-update area-ov)) )
    (remove-hook 'window-scroll-functions      upd-if-finc)
    (remove-hook 'window-size-change-functions update-func) ))

(defun demap-area-ov-set(area-ov window)
  ""
  (if (not window)
      (overlay-put (demap-area-ov-overlay area-ov) 'face 'demap-visible-region-inactive-face )
    (overlay-put (demap-area-ov-overlay area-ov) 'face 'demap-visible-region-face)
    (setf (demap-area-ov-window area-ov) window)
    (let ((upd-if-finc (apply-partially #'demap-area-ov-update-if-window area-ov))
          (update-func (apply-partially #'demap-area-ov-update area-ov)) )
      (add-hook 'window-scroll-functions      upd-if-finc)
      (add-hook 'window-size-change-functions update-func) )
    (demap-area-ov-update area-ov) ))


;;;track-w-------

(cl-defstruct (demap-track-w
               (:copier nil)
               (:constructor nil)
               (:constructor demap-track-w-construct-unhooked
                (&optional
                 (minimap (demap-minimap-construct))
                 (line-ov (demap-line-ov-construct minimap))
                 (area-ov (demap-area-ov-construct minimap)) )))
  (minimap nil :read-only t)
  (line-ov nil)
  (area-ov nil) )


(defun demap-track-w-test-window-p(track-w);TODO: redo doc for all in view update
  "Determin if demap-minimap TRACK-W can fallow the current window."
  (ignore track-w)
  (buffer-file-name (window-buffer)) )

;;track-w window

(defun demap-track-w-window-set(track-w window)
  ""
  ;;TODO clean with when-let
  (let ((minimap (demap-track-w-minimap track-w))
        (line-ov (demap-track-w-line-ov track-w))
        (area-ov (demap-track-w-area-ov track-w)) )
    (when window
      (setf (demap-minimap-showing minimap) (window-buffer window)) )
    (when line-ov
      (demap-line-ov-set line-ov window) )
    (when area-ov
      (demap-area-ov-set area-ov window) )))

;;track-w update

(defun demap-track-w-update(track-w)
  "Update whet window demap-minimap TRACK-W is showing."
  (demap-track-w-window-set track-w (when (demap-track-w-test-window-p track-w)
                                      (selected-window) )))

;;track-w hook

(defun demap-track-w-unhook(track-w)
  ""
  (let ((update-func (apply-partially #'demap-track-w-update track-w))
        (unhook-func (apply-partially #'demap-track-w-unhook track-w))
        (buffer  (demap-minimap-buffer (demap-track-w-minimap track-w)))
        (line-ov (demap-track-w-line-ov track-w))
        (area-ov (demap-track-w-area-ov track-w)) )
    (demap--tools-remove-hook       'window-state-change-hook update-func)
    (demap--tools-remove-hook-local 'demap-minimap-kill-hook  unhook-func buffer)
    (when line-ov
      (demap-line-ov-unset line-ov) )
    (when area-ov
      (demap-area-ov-unset area-ov) ) ))

(defun demap-track-w-hook(track-w)
  ""
  (let ((update-func (apply-partially #'demap-track-w-update track-w))
        (unhook-func (apply-partially #'demap-track-w-unhook track-w))
        (buffer      (demap-minimap-buffer (demap-track-w-minimap track-w))) )
    (add-hook 'window-state-change-hook update-func)
    (demap--tools-add-hook-local 'demap-minimap-kill-hook unhook-func nil buffer) ))

;;track-w construct

;;;###autoload
(defun demap-track-w-construct()
  ""
  (let ((track-w (demap-track-w-construct-unhooked)))
    (demap-track-w-hook track-w)
    track-w))

;;;###autoload
(defun demap-test()
  ""
  (x-focus-frame nil)
  (let ((window  (split-window-right))
        (track-w (demap-track-w-construct)) )
    (set-window-buffer window (demap-minimap-buffer (demap-track-w-minimap track-w))) ))



(provide 'demap-track-w)
;;; demap-track-w.el ends here
