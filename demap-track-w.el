;;; demap-track-w.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://gitlab.com/sawyerjgardner>
;; Maintainer: Sawyer Gardner <sawyerjgardner@gmail.com>
;; Created: January 04, 2022
;; Modified: January 05, 2022
;; Version: 1.0.0
;; Keywords: convenience extensions lisp
;; Homepage: https://gitlab.com/sawyerjgardner/demap.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Addon to demap-minimap that adds the ability for a minimap to fallow the
;; selected window and an overlay that shows that windows visible region and its
;; current line.
;;
;;
;; this file includes
;;   faces:
;;     `demap-current-line-face'
;;     `demap-current-line-inactive-face'
;;     `demap-visible-region-face'
;;     `demap-visible-region-inactive-face'
;;   variables:
;;     `demap-track-w-update-p-func'
;;   interactive:
;;     `demap-track-w-construct'
;;
;;   demap-line-ov:
;;     `demap-line-ov-construct'
;;     `demap-line-ov-minimap'
;;     `demap-line-ov-overlay'
;;     `demap-line-ov-window'
;;     `demap-line-ov-update'
;;     `demap-line-ov-deactivate'
;;     `demap-line-ov-activate'
;;     `demap-line-ov-set'
;;   demap-area-ov:
;;     `demap-area-ov-construct'
;;     `demap-area-ov-minimap'
;;     `demap-area-ov-overlay'
;;     `demap-area-ov-window'
;;     `demap-area-ov-update'
;;     `demap-area-ov-deactivate'
;;     `demap-area-ov-activate'
;;     `demap-area-ov-set'
;;   demap-track-w:
;;     `demap-track-w-construct'
;;     `demap-track-w-minimap'
;;     `demap-track-w-line-ov'
;;     `demap-track-w-area-ov'
;;     `demap-track-w-construct-unhooked'
;;     `demap-track-w-update-p-func-defalt'
;;     `demap-track-w-window-set'
;;     `demap-track-w-update'
;;     `demap-track-w-unhook'
;;     `demap-track-w-hook'
;;
;;
;;; Code:


(require 'cl-lib)
(require 'demap--tools)
(require 'demap-minimap)


(defface demap-visible-region-face
  '((((background dark)) (:background "gray10" :extend t))
    (t (:background "gray10" :extend t)))
  "Face used to highlight visible region in demap-minimap."
  :group 'demap)

(defface demap-visible-region-inactive-face
  '((((background dark)) (:background "gray16" :extend t))
    (t (:background "gray16" :extend t)))
  "Face used to highlight visible region in demap-minimap when not active."
  :group 'demap)

(defface demap-current-line-face
  '((((background dark)) (:background "gray50" :extend t))
    (t (:background "gray50" :extend t)))
  "Face used to highlight the current line in demap-minimap."
  :group 'demap)

(defface demap-current-line-inactive-face
  '((((background dark)) (:background "gray77" :extend t))
    (t (:background "gray77" :extend t)))
  "Face used to highlight the current line in demap-minimap when not active."
  :group 'demap)

(defcustom demap-track-w-update-p-func #'demap-track-w-update-p-func-defalt
  "Function to determin if demap-minimap should show the selected window.
the function should accept one argument (TRACK-W)
and return nil if TRACK-W should not show the
selected window. this function is called with
TRACK-W's minimap's buffer has the current buffer."
  :type 'function
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
  "Move LINE-OV's overlay to its window's current line in its minimap.
if LINE-OV's window is no longer selected then
`demap-line-ov-deactivate' is called."
  (if (eq (selected-window) (demap-line-ov-window line-ov))
      (move-overlay (demap-line-ov-overlay line-ov)
                    (line-beginning-position)
                    (+ (line-end-position) 1)
                    (demap-minimap-buffer (demap-line-ov-minimap line-ov)) )
    (demap-line-ov-deactivate line-ov) ))

;;line-ov active

(defun demap-line-ov-deactivate(line-ov)
  "Deactivate LINE-OV.
hooks are removed and LINE-OV changes face."
  (overlay-put (demap-line-ov-overlay line-ov) 'face 'demap-current-line-inactive-face)
  (remove-hook 'post-command-hook (apply-partially #'demap-line-ov-update line-ov)) )

(defun demap-line-ov-activate(line-ov)
  "Activate LINE-OV.
hooks are added for updating LINE-OV and overlay
face is set."
  (overlay-put (demap-line-ov-overlay line-ov) 'face 'demap-current-line-face)
  (add-hook 'post-command-hook (apply-partially #'demap-line-ov-update line-ov))
  (demap-line-ov-update line-ov) )

(defun demap-line-ov-set(line-ov window)
  "Activate LINE-OV's and set window to WINDOW.
LINE-OV will use WINDOW for updates. if WINDOW is
nil, this function dose nuthing."
  (when window
    (setf (demap-line-ov-window line-ov) window)
    (demap-line-ov-activate line-ov)))


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


(defun demap--area-ov-active-p(area-ov)
  "Determin if AREA-OV should be active or not."
  (let ((window  (demap-area-ov-window  area-ov))
        (minimap (demap-area-ov-minimap area-ov)) )
    (and (window-live-p window)
         (eq (demap--tools-real-buffer (window-buffer window)) (demap-minimap-showing minimap)) )))

;;area-ov center

(defun demap--area-ov-make-visible(area-ov)
  "Scroll AREA-OV's minimap so that its overlay is visible."
  (let ((overlay  (demap-area-ov-overlay area-ov))
        (buffer   (demap-minimap-buffer (demap-area-ov-minimap area-ov))) )
    (dolist (window (get-buffer-window-list buffer t t))
      (when (>= (overlay-end overlay) (window-end window))
        (set-window-point window (overlay-end overlay)) )
      (when (<= (overlay-start overlay) (window-start window))
        (set-window-point window (- (overlay-start overlay) 1)) ))))

;;area-ov update

(defun demap-area-ov-update(area-ov &optional &rest r)
  "Move AREA-OV's overlay to its window's visible region in its minimap.
if AREA-OV's window is not showing what AREA-OV's
minimap is showing, then `demap-area-ov-deactivate'
is called.
R is ignored so that this function can be passed to
function hooks."
  (ignore r)
  (let ((window (demap-area-ov-window area-ov)))
    (if (demap--area-ov-active-p area-ov)
        (progn
          (move-overlay (demap-area-ov-overlay area-ov)
                        (window-start window)
                        (window-end window t)
                        (demap-minimap-buffer (demap-area-ov-minimap area-ov)) )
          (demap--area-ov-make-visible area-ov) )
      (demap-area-ov-deactivate area-ov) )))

(defun demap--area-ov-update-if-window(area-ov window &optional &rest r)
  "Update AREA-OV's window is WINDOW.
R is ignored so that this function can be passed to
function hooks."
  (ignore r)
  (when (eq (demap-area-ov-window area-ov) window)
    (demap-area-ov-update area-ov) ))

;;area-ov set

(defun demap-area-ov-deactivate(area-ov)
  "Deactivate AREA-OV.
AREA-OV's update hooks are removed."
  (let ((upd-if-finc (apply-partially #'demap--area-ov-update-if-window area-ov))
        (update-func (apply-partially #'demap-area-ov-update area-ov)) )
    (remove-hook 'window-scroll-functions      upd-if-finc)
    (remove-hook 'window-size-change-functions update-func) ))

(defun demap-area-ov-activate(area-ov)
  "Activate AREA-OV.
update hooks are set and overlays face is set."
  (overlay-put (demap-area-ov-overlay area-ov) 'face 'demap-visible-region-face)
  (let ((upd-if-finc (apply-partially #'demap--area-ov-update-if-window area-ov))
        (update-func (apply-partially #'demap-area-ov-update area-ov)) )
    (add-hook 'window-scroll-functions      upd-if-finc)
    (add-hook 'window-size-change-functions update-func) )
  (demap-area-ov-update area-ov) )

(defun demap-area-ov-set(area-ov window)
  "Set AREA-OV's window to WINDOW and activate.
if WINDOW is nil then set AREA-OV's overlay to the
inactive face but keep update hooks."
  (if (not window)
      (overlay-put (demap-area-ov-overlay area-ov) 'face 'demap-visible-region-inactive-face )
    (setf (demap-area-ov-window area-ov) window)
    (demap-area-ov-activate area-ov) ))


;;;track-w-------

(cl-defstruct (demap-track-w
               (:copier nil)
               (:constructor nil)
               (:constructor demap-track-w-construct-unhooked
                (&key
                 (minimap (demap-minimap-construct))
                 (line-ov (demap-line-ov-construct minimap))
                 (area-ov (demap-area-ov-construct minimap)) )))
  (minimap nil :read-only t)
  (line-ov nil)
  (area-ov nil) )


;;track-w window

(defun demap-track-w-window-set(track-w window)
  "Set TRACK-W to show WINDOW.
updates TRACK-W's minimap and overlays accordingly.
if WINDOW is nil then TRACK-W is made inactive."
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

(defun demap-track-w-update-p-func-defalt(track-w)
  "Determin if TRACK-W can fallow the current window."
  (ignore track-w)
  (buffer-file-name (window-buffer)) )

(defun demap--track-w-update-p-func-call(track-w)
  "Determin if TRACK-W can fallow the current window."
  (with-current-buffer (demap-minimap-buffer (demap-track-w-minimap track-w))
    (funcall demap-track-w-update-p-func track-w)) )

(defun demap-track-w-update(track-w)
  "Update TRACK-W to test and show current window.
if the current window fails `demap-track-w-window-set'
 then track-w is deactivated."
  (demap-track-w-window-set track-w (when (demap--track-w-update-p-func-call track-w)
                                      (selected-window) )))

;;track-w hook

(defun demap-track-w-unhook(track-w)
  "Remove all update hooks setup by TRACK-W and its overlays."
  (let ((update-func (apply-partially #'demap-track-w-update track-w))
        (unhook-func (apply-partially #'demap-track-w-unhook track-w))
        (buffer  (demap-minimap-buffer (demap-track-w-minimap track-w)))
        (line-ov (demap-track-w-line-ov track-w))
        (area-ov (demap-track-w-area-ov track-w)) )
    (demap--tools-remove-hook       'window-state-change-hook update-func)
    (demap--tools-remove-hook-local 'demap-minimap-kill-hook  unhook-func buffer)
    (when line-ov
      (demap-line-ov-deactivate line-ov) )
    (when area-ov
      (demap-area-ov-deactivate area-ov) ) ))

(defun demap-track-w-hook(track-w)
  "Setup hooks to update TRACK-W when windows change."
  (let ((update-func (apply-partially #'demap-track-w-update track-w))
        (unhook-func (apply-partially #'demap-track-w-unhook track-w))
        (buffer      (demap-minimap-buffer (demap-track-w-minimap track-w))) )
    (add-hook 'window-state-change-hook update-func)
    (demap--tools-add-hook-local 'demap-minimap-kill-hook unhook-func nil buffer) ))

;;track-w construct

;;;###autoload
(defun demap-track-w-construct(&rest args);;TODO: improve doc
  "Constructor for objects of type `demap-track-w'.
ARGS are passed to `demap-track-w-construct-unhooked'.

\(fn &key (MINIMAP (demap-minimap-construct)) (LINE-OV (demap-line-ov-construct minimap)) (AREA-OV (demap-area-ov-construct minimap)))"
  (interactive)
  (let ((track-w (apply #'demap-track-w-construct-unhooked args)))
    (demap-track-w-hook track-w)
    track-w ))


(provide 'demap-track-w)
;(provide 'demap)
;;; demap-track-w.el ends here
