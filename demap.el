;;; demap.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://github.com/sawyer>
;; Maintainer: Sawyer Gardner <sawyerjgardner@gmail.com>
;; Created: November 25, 2021
;; Modified: November 25, 2021
;; Version: 0.0.1
;; Keywords: extensions lisp tools
;; Homepage: https://github.com/sawyer/demap
;; Package-Requires: ((emacs "24.4") (dash "2.18.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(require 'demap-minimap)
(require 'demap-modes)
(require 'dash)
(require 'cl-lib)


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
    0        for any visible.

the side and width of the new window can be set
with `demap-set-open-options' or adding a display
action to `display-buffer-alist' for minimaps."
  (interactive)
  (-> minimap-or-name
      (or (get-buffer demap-minimap-defalt-name)
          (demap-minimap-construct) )
      (demap-normalize-minimap)
      (demap-minimap-buffer)
      (display-buffer '((display-buffer-in-side-window)
                        (side . right)
                        (window-width . 20) )
                      frame )))

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

if a window is removed returns t, otherwise nil."
  (interactive)
  (cl-dolist (window
              (-some->
                  (-> (or minimap-or-name
                          demap-minimap-defalt-name )
                      (demap-normalize-minimap)
                      (ignore-errors) )
                (-> (demap-minimap-buffer)
                    (get-buffer-window-list nil frame) ))
              nil )
    (when (or (window-parameter window 'window-side)
              (-some-> (window-parent window)
                (window-parameter 'window-side) ))
      (delete-window window)
      (cl-return t) )))

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
    (demap-minimap-open minimap-or-name frame)))


(defun demap-minimap-buffer-display-condition(buffer &optional act)
  ""
  (ignore act)
  (demap-buffer-minimap buffer) )

(defun demap-set-open-options(&optional side width)
  "Set options for how to display a demap-minimap buffer.
adds settings to `display-buffer-alist' for
demap-minimap buffers, changing how
`demap-minimap-open' opens the minimap.

SIDE  is the side of the frame the window should be,
        can be 'left or 'right (defalts to 'right).
WIDTH is the width of the window, should be an
        integer unit or a float percentage (defalts
        to 20)."
  (setq side  (or side 'right)
        width (or width 20) )
  (--> #'demap-minimap-buffer-display-condition
       (assq-delete-all it display-buffer-alist)
       (setq display-buffer-alist it))
  (push `(#'demap-minimap-buffer-display-condition
          (display-buffer-in-side-window)
          (side         . ,side)
          (window-width . ,width) )
        display-buffer-alist ))

(provide 'demap)
;;; demap.el ends here
