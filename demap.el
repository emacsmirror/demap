;;; demap.el --- Detachable minimap package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://gitlab.com/sawyerjgardner>
;; Created: November 25, 2021
;; Modified: February 04, 2022
;; Version: 1.0.0
;; Keywords: lisp tools convenience
;; Homepage: https://gitlab.com/sawyerjgardner/demap.el
;; Package-Requires: ((emacs "24.4") (dash "2.18.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; this package adds a minimap that shows a zoomed out view of the active window's
;; buffer. you can toggle showing the minimap in a side window with
;; `demap-minimap-toggle'.
;;
;; this package has a few advantages over other minimap packages.
;; - support for detaching minimaps and having them on a diffrent frames then the
;;      active window.
;; - support for multiple minimap buffer, with there own buffer local definitions
;;      on what buffers it can show and how to show them.
;; - person preference, having the minimap on the side of the frame rather
;;      then on the side of the active window.
;;
;; see the README for more information on the demap package and customization
;; options.
;;
;;; Code:

(require 'demap--tools)
(demap--tools-define-demap-start)
(require 'demap-minimap)
(require 'demap-modes)
(require 'dash)
(require 'cl-lib)

(defcustom demap-minimap-close-kill-minimap-p t
  "Whether `demap-minimap-close' can kill minimap buffers.
`demap-minimap-close' will only kill the minimap
buffer if it is not in any other window."
  :package-version '(demap . "1.0.0")
  :type  'boolean
  :group 'demap)

(defcustom demap-minimap-window-side 'right
  "The side of the frame `demap-minimap-open' opens a window on."
  :package-version '(demap . "1.0.0")
  :type  '(radio (const right)
                 (const left) )
  :group 'demap)

(defcustom demap-minimap-window-width 20
  "The width of the window `demap-minimap-open' opens."
  :package-version '(demap . "1.0.0")
  :type  'number
  :group 'demap)

;;;###autoload
(defun demap-minimap-open(&optional minimap-or-name frame)
  "Open minimap in a side window.
makes a minimap buffer and shows it. if
MINIMAP-OR-NAME is non-nil or a minimap with the
name in `demap-minimap-defalt-name' exists, show
that minimap instead. if the minimap is already
being shown, nuthing happens.

FRAME specifies what frame to look for windows
that already show the minimap. it should be a live
frame or one of the fallowing:
    nil      for the selected frame,
    t        for any live frame,
    'visible for any visible frame,
    0        for any visible."
  (interactive) ;TODO: option for different minimaps for ech frame.
  (let ((display-buffer-overriding-action `((display-buffer-in-side-window)
                                            (side          . ,demap-minimap-window-side)
                                            (window-width  . ,demap-minimap-window-width)
                                            (preserve-size . (t . nil)) )))
    (-> (or minimap-or-name
            (get-buffer demap-minimap-defalt-name)
            (demap-minimap-construct) )
        (demap-normalize-minimap)
        (demap-minimap-buffer)
        (display-buffer nil frame) )))

;;;###autoload
(defun demap-minimap-close(&optional minimap-or-name frame)
  "Close side window showing a minimap.
close a side window showing MINIMAP-OR-NAME. has no
effect on normal windows showing MINIMAP-OR-NAME.

a side window is a window made by
`display-buffer-in-side-window' (the defalt method
used by `demap-minimap-open').

FRAME specifies what frame to look for side windows
showing a minimap. it should be a live frame or one
of the fallowing:
    nil      for the selected frame,
    t        for any live frame,
    'visible for any visible frame,
    0        for any visible.

could kill MINIMAP-OR-NAME if
`demap-minimap-close-kill-minimap-p' is non-nil.

if a window is removed returns t, otherwise nil."
  (interactive)
  (-when-let (minimap-buffer (ignore-errors
                               (-> (or minimap-or-name
                                       demap-minimap-defalt-name )
                                   (demap-normalize-minimap)
                                   (demap-minimap--buffer) )))
    (cl-dolist (window (get-buffer-window-list minimap-buffer nil frame) nil)
      (when (demap--tools-side-window-p window)
        (delete-window window)
        (when (and demap-minimap-close-kill-minimap-p
                   (not (get-buffer-window minimap-buffer t)) )
          (kill-buffer minimap-buffer) )
        (cl-return t) ))))

;;;###autoload
(defun demap-minimap-toggle(&optional minimap-or-name frame)
  "Toggle side window showing a minimap.
opens MINIMAP-OR-NAME in a side window. if its
already showing, removes it instead.

FRAME specifies what frame to look for side windows
showing a minimap. it should be a live frame or one
of the fallowing:
    nil      for the selected frame,
    t        for any live frame,
    'visible for any visible frame,
    0        for any visible.

see `demap-minimap-open' and `demap-minimap-close'
for more information."
  (interactive)
  (unless (demap-minimap-close minimap-or-name frame)
    (demap-minimap-open minimap-or-name frame) ))


(provide 'demap)
;;; demap.el ends here
