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




(defvar-local demap-track-w-mode nil
  "")

(defcustom demap-track-w-mode-update-p-func #'demap-track-w-mode-update-p-func-defalt
  "Function to determin if demap-minimap should show the selected window.
the function should accept one argument (TRACK-W)
and return nil if TRACK-W should not show the
selected window. this function is called with
TRACK-W's minimap's buffer has the current buffer."
  :type 'function
  :group 'demap)


(defvar-local demap-current-line-mode nil
  "")

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


(defvar-local demap-test-area-mode nil
  "")

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



;;;track-w-mode-------

;;track-w-mode update

(defun demap-track-w-mode-update-p-func-defalt()
  "Determin if TRACK-W can fallow the current window."
  (buffer-file-name (window-buffer)) )

(defun demap-track-w-mode-update()
  "Update TRACK-W to test and show current window.
if the current window fails `demap-track-w-window-set'
 then track-w is deactivated."
  (if (funcall demap-track-w-mode-update-p-func)
      (setf (demap-current-minimap-window) (selected-window))
    (demap-current-minimap-window-sleep) ))

(defun demap-track-w-mode-update-as(minimap)
  ""
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap-track-w-mode-update) ))

;;track-w-mode

(defun demap--track-w-mode-init()
  ""
  (setf demap-track-w-mode t)
  (add-hook 'window-state-change-hook (apply-partially #'demap-track-w-mode-update-as (demap-buffer-minimap)))
  (add-hook 'demap-minimap-kill-hook  #'demap--track-w-mode-kill nil t) )

(defun demap--track-w-mode-kill()
  ""
  (remove-hook 'window-state-change-hook (apply-partially #'demap-track-w-mode-update-as (demap-buffer-minimap)))
  (remove-hook 'demap-minimap-kill-hook  #'demap--track-w-mode-kill t)
  (kill-local-variable 'demap-track-w-mode) )

(defun demap--track-w-mode-set(state)
  ""
  (cl-assert (demap-buffer-minimap) nil "buffer is not a demap-minimap-buffer: %s" (current-buffer))
  (when (xor state demap-track-w-mode)
    (if state
        (demap--track-w-mode-init)
      (demap--track-w-mode-kill) )
    (message "-- %s" state) ))

(defun demap--track-w-mode-val() "." demap-track-w-mode)
(gv-define-simple-setter demap--track-w-mode-val demap--track-w-mode-set)
(define-minor-mode demap-track-w-mode
  "hoho"
  :group 'demap
  :variable (demap--track-w-mode-val))


;;;demap-current-line-mode-------

;;demap current-line update

(defun demap--current-line-mode-update()
  ""
  (let ((w-buffer (window-buffer (demap-current-minimap-window)))
        (m-buffer (current-buffer))
        (showing  (demap-minimap-showing (demap-buffer-minimap)))
        (ov       demap-current-line-mode) )
    (when (eq showing (demap--tools-real-buffer w-buffer))
      (with-current-buffer w-buffer
        (move-overlay ov
                      (line-beginning-position)
                      (+ (line-end-position) 1)
                      m-buffer )))))

(defun demap--current-line-mode-update-has(minimap)
  ""
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--current-line-mode-update) ))

;;current-line activate

(defun demap--current-line-mode-activate()
  ""
  (overlay-put demap-current-line-mode 'face 'demap-current-line-face)
  (add-hook 'post-command-hook (apply-partially #'demap--current-line-mode-update-has (demap-buffer-minimap)))
  (demap--current-line-mode-update) )

(defun demap--current-line-mode-deactivate()
  ""
  (overlay-put demap-current-line-mode 'face 'demap-current-line-inactive-face)
  (remove-hook 'post-command-hook (apply-partially #'demap--current-line-mode-update-has (demap-buffer-minimap))) )

(defun demap--current-line-mode-activate-if()
  ""
  (if (demap-current-minimap-window)
      (demap--current-line-mode-activate)
    (demap--current-line-mode-deactivate) ))

;;current-line mode

(defun demap--current-line-mode-init()
  ""
  (setq demap-current-line-mode (make-overlay 0 0))
  (demap-minimap-protect-variables t 'demap-current-line-mode)
  (add-hook 'demap-minimap-window-set-hook   #'demap--current-line-mode-activate-if nil t)
  (add-hook 'demap-minimap-window-sleep-hook #'demap--current-line-mode-deactivate  nil t)
  (add-hook 'demap-minimap-kill-hook         #'demap--current-line-mode-kill        nil t) )

(defun demap--current-line-mode-kill()
  ""
  (demap--current-line-mode-deactivate)
  (delete-overlay demap-current-line-mode)
  (kill-local-variable 'demap-current-line-mode)
  (demap-minimap-unprotect-variables t 'demap-current-line-mode)
  (remove-hook 'demap-minimap-window-set-hook   #'demap--current-line-mode-activate-if t)
  (remove-hook 'demap-minimap-window-sleep-hook #'demap--current-line-mode-deactivate  t)
  (remove-hook 'demap-minimap-kill-hook         #'demap--current-line-mode-kill        t) )

(defun demap--current-line-mode-set(state)
  ""
  (when (xor state demap-current-line-mode)
    (if state
        (demap--current-line-mode-init)
      (demap--current-line-mode-kill) )
    (message "-- line - %s" state) ))

(define-minor-mode demap-current-line-mode
  "hoho"
  :group 'demap
  :variable (demap-current-line-mode . demap--current-line-mode-set))


;;;demap-test-area-mode-------

;;demap test-area update

(defun demap--test-area-made-active-p()
  "Determin if AREA-OV should be active or not."
  (let ((window  (demap-current-minimap-window))
        (showing (demap-minimap-showing (demap-buffer-minimap))) )
    (and (window-live-p window)
         (eq (demap--tools-real-buffer (window-buffer window)) showing) )))

(defun demap--test-area-mode-update()
  ""
  (let ((window (demap-current-minimap-window)))
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
  (when (eq window (demap-minimap-window minimap) )
    (with-current-buffer (demap-minimap-buffer minimap)
      (demap--test-area-mode-update) )))

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

(defun demap--test-area-mode-activate-if()
  ""
  (if (demap-current-minimap-window)
      (demap--test-area-mode-activate)
    (demap--test-area-mode-deactivate) ))

(defun demap--test-area-mode-sleep()
  ""
  (overlay-put demap-test-area-mode 'face 'demap-visible-region-inactive-face) )

;;test-area mode

(defun demap--test-area-mode-init()
  ""
  (setq demap-test-area-mode (make-overlay 0 0))
  (demap-minimap-protect-variables t 'demap-test-area-mode)
  (add-hook 'demap-minimap-window-set-hook   #'demap--test-area-mode-activate-if nil t)
  (add-hook 'demap-minimap-window-sleep-hook #'demap--test-area-mode-sleep       nil t)
  (add-hook 'demap-minimap-kill-hook         #'demap--test-area-mode-kill        nil t) )

(defun demap--test-area-mode-kill()
  ""
  (demap--test-area-mode-deactivate)
  (delete-overlay demap-test-area-mode)
  (kill-local-variable 'demap-test-area-mode)
  (demap-minimap-unprotect-variables t 'demap-test-area-mode)
  (remove-hook 'demap-minimap-window-set-hook   #'demap--test-area-mode-activate-if t)
  (remove-hook 'demap-minimap-window-sleep-hook #'demap--test-area-mode-sleep       t)
  (remove-hook 'demap-minimap-kill-hook         #'demap--test-area-mode-kill        t) )

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
