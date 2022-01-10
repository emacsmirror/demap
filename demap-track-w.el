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


(defvar-local demap-minimap-window nil
  "")


(defvar-local demap-test-mode nil
  "")

(defvar demap-test-mode-window-set-functions nil
  "Take one argument (window).")

(defcustom demap-test-mode-update-p-func #'demap-test-mode-update-p-func-defalt
  "Function to determin if demap-minimap should show the selected window.
the function should accept one argument (TRACK-W)
and return nil if TRACK-W should not show the
selected window. this function is called with
TRACK-W's minimap's buffer has the current buffer."
  :type 'function
  :group 'demap)


(defvar-local demap-test-line-mode nil
  "")

(defvar-local demap-test-area-mode nil
  "")


;;;test-mode-------

;;test-mode window

(defun demap--test-mode-window-set(window)
  "Set TRACK-W to show WINDOW.
updates TRACK-W's minimap and overlays accordingly.
if WINDOW is nil then TRACK-W is made inactive."
  (when window
    (setf demap-minimap-window window
          (demap-minimap-showing (demap-buffer-minimap)) (window-buffer window)) )
  (with-demoted-errors "error in demap-window-set-functions: %s"
    (run-hook-with-args 'demap-test-mode-window-set-functions window) ))

;;test-mode update

(defun demap-test-mode-update-p-func-defalt()
  "Determin if TRACK-W can fallow the current window."
  (buffer-file-name (window-buffer)) )

(defun demap-test-mode-update()
  "Update TRACK-W to test and show current window.
if the current window fails `demap-track-w-window-set'
 then track-w is deactivated."
  (demap--test-mode-window-set (when (funcall demap-test-mode-update-p-func)
                                 (selected-window) )))

(defun demap-test-mode-update-as(minimap)
  ""
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap-test-mode-update) ))

;;test-mode

(defun demap--test-mode-init()
  ""
  (setf demap-test-mode t)
  (add-hook 'window-state-change-hook (apply-partially #'demap-test-mode-update-as (demap-buffer-minimap)))
  (add-hook 'demap-minimap-kill-hook  #'demap--test-mode-kill nil t)
  (demap-minimap-protect-variables t 'demap-test-mode 'demap-test-mode-window-set-functions) )

(defun demap--test-mode-kill()
  ""
  (remove-hook 'window-state-change-hook (apply-partially #'demap-test-mode-update-as (demap-buffer-minimap)))
  (remove-hook 'demap-minimap-kill-hook  #'demap--test-mode-kill t)
  (demap-minimap-unprotect-variables t 'demap-test-mode 'demap-test-mode-window-set-functions)
  (kill-local-variable 'demap-test-mode) )

(defun demap--test-mode-set(state)
  ""
  (cl-assert (demap-buffer-minimap) nil "buffer is not a demap-minimap-buffer: %s" (current-buffer))
  (when (xor state demap-test-mode)
    (if state
        (demap--test-mode-init)
      (demap--test-mode-kill) )
    (message "-- %s" state) ))

(defun demap--test-mode-val() "." demap-test-mode)
(gv-define-simple-setter demap--test-mode-val demap--test-mode-set)
(define-minor-mode demap-test-mode
  "hoho"
  nil
  nil
  :group 'demap
  :variable (demap--test-mode-val))


;;;demap-test-line-mode-------

;;demap test-line update

(defun demap--test-line-mode-update()
  ""
  (let ((window demap-minimap-window)
        (ov     demap-test-line-mode)
        (buffer (current-buffer)) )
    (if (eq (selected-window) window)
        (with-current-buffer (window-buffer window)
          (move-overlay ov
                        (line-beginning-position)
                        (+ (line-end-position) 1)
                        buffer ))
      (demap--test-line-mode-deactivate) )))

(defun demap--test-line-mode-update-has(minimap)
  ""
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--test-line-mode-update) ))

;;test-line activate

(defun demap--test-line-mode-activate()
  ""
  (overlay-put demap-test-line-mode 'face 'demap-current-line-face)
  (add-hook 'post-command-hook (apply-partially #'demap--test-line-mode-update-has (demap-buffer-minimap)))
  (demap--test-line-mode-update) )

(defun demap--test-line-mode-deactivate()
  ""
  (overlay-put demap-test-line-mode 'face 'demap-current-line-inactive-face)
  (remove-hook 'post-command-hook (apply-partially #'demap--test-line-mode-update-has (demap-buffer-minimap))) )

(defun demap--test-line-mode-activate-when(window)
  ""
  (when window
    (demap--test-line-mode-activate) ))

;;test-line mode

(defun demap--test-line-mode-init()
  ""
  (setq demap-test-line-mode (make-overlay 0 0))
  (demap-minimap-protect-variables t 'demap-test-line-mode)
  (add-hook 'demap-test-mode-window-set-functions #'demap--test-line-mode-activate-when nil t)
  (add-hook 'demap-minimap-kill-hook #'demap--test-line-mode-kill nil t) )

(defun demap--test-line-mode-kill()
  ""
  (demap--test-line-mode-deactivate)
  (delete-overlay demap-test-line-mode)
  (kill-local-variable 'demap-test-line-mode)
  (demap-minimap-unprotect-variables t 'demap-test-line-mode)
  (remove-hook 'demap-test-mode-window-set-functions #'demap--test-line-mode-activate-when t)
  (remove-hook 'demap-minimap-kill-hook #'demap--test-line-mode-kill t) )

(defun demap--test-line-mode-set(state)
  ""
  (when (xor state demap-test-line-mode)
    (if state
        (demap--test-line-mode-init)
      (demap--test-line-mode-kill) )
    (message "-- line - %s" state) ))

(define-minor-mode demap-test-line-mode
  "hoho"
  :group 'demap
  :variable (demap-test-line-mode . demap--test-line-mode-set))


;;;demap-test-area-mode-------

;;demap test-area update

(defun demap--test-area-made-active-p()
  "Determin if AREA-OV should be active or not."
  (let ((window  demap-minimap-window)
        (showing (demap-minimap-showing (demap-buffer-minimap))) )
    (and (window-live-p window)
         (eq (demap--tools-real-buffer (window-buffer window)) showing) )))

(defun demap--test-area-mode-update()
  ""
  (let ((window demap-minimap-window))
    (if (demap--test-area-made-active-p)
        (move-overlay demap-test-area-mode
                      (window-start window)
                      (window-end window t)
                      (current-buffer) )
      (demap--test-area-mode-deactivate) )))

(defun demap--test-area-mode-update-has(minimap &rest i)
  ""
  (ignore i)
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--test-area-mode-update) ))

(defun demap--test-area-mode-update-window-as(minimap window &rest i)
  ""
  (ignore i)
  (let ((buffer (demap-minimap-buffer minimap)))
    (when (eq window (buffer-local-value 'demap-minimap-window buffer))
      (with-current-buffer buffer
        (demap--test-area-mode-update) ))))

;;test-area activate

(defun demap--test-area-mode-activate()
  ""
  (overlay-put demap-test-area-mode 'face 'demap-visible-region-face)
  (let ((scrl-func (apply-partially #'demap--test-area-mode-update-window-as (demap-buffer-minimap)))
        (size-func (apply-partially #'demap--test-area-mode-update-has       (demap-buffer-minimap))) )
    (add-hook 'window-scroll-functions      scrl-func)
    (add-hook 'window-size-change-functions size-func) )
  (demap--test-area-mode-update) )

(defun demap--test-area-mode-deactivate()
  ""
  (overlay-put demap-test-area-mode 'face 'demap-visible-region-inactive-face)
  (let ((scrl-func (apply-partially #'demap--test-area-mode-update-window-as (demap-buffer-minimap)))
        (size-func (apply-partially #'demap--test-area-mode-update-has       (demap-buffer-minimap))) )
    (remove-hook 'window-scroll-functions      scrl-func)
    (remove-hook 'window-size-change-functions size-func) ))

(defun demap--test-area-mode-activate-when(window)
  ""
  (when window
    (demap--test-area-mode-activate) ))

;;test-area mode

(defun demap--test-area-mode-init()
  ""
  (setq demap-test-area-mode (make-overlay 0 0))
  (demap-minimap-protect-variables t 'demap-test-area-mode)
  (add-hook 'demap-test-mode-window-set-functions #'demap--test-area-mode-activate-when nil t)
  (add-hook 'demap-minimap-kill-hook #'demap--test-area-mode-kill nil t) )

(defun demap--test-area-mode-kill()
  ""
  (demap--test-area-mode-deactivate)
  (delete-overlay demap-test-area-mode)
  (kill-local-variable 'demap-test-area-mode)
  (demap-minimap-unprotect-variables t 'demap-test-area-mode)
  (remove-hook 'demap-test-mode-window-set-functions #'demap--test-area-mode-activate-when t)
  (remove-hook 'demap-minimap-kill-hook #'demap--test-area-mode-kill t) )

(defun demap--test-area-mode-set(state)
  ""
  (when (xor state demap-test-area-mode)
    (if state
        (demap--test-area-mode-init)
      (demap--test-area-mode-kill) )
    (message "-- area - %s" state) ))

(define-minor-mode demap-test-area-mode
  "hoho"
  :group 'demap
  :variable (demap-test-area-mode . demap--test-area-mode-set))



(provide 'demap-track-w)
;(provide 'demap)
;;; demap-track-w.el ends here
