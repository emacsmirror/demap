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
;; Package-Requires: ((emacs "24.4"))
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
(require 'cl-lib)



;;;###autoload
(defun demap-minimap-open(&optional minimap-or-name frame)
  ""
  (interactive)
  (setq minimap-or-name (or minimap-or-name
                            (get-buffer demap-minimap-defalt-name)
                            (demap-minimap-construct) ))
  (let* ((minimap (demap-normalize-minimap minimap-or-name))
         (window (display-buffer (demap-minimap-buffer minimap)
                                 '((display-buffer-in-side-window)
                                   (side . right)
                                   (window-width . 20) )
                                 frame) ))
    window ))

;;;###autoload
(defun demap-minimap-close(&optional minimap-or-name frame)
  ""
  (interactive)
  (let* ((minimap (ignore-errors
                    (demap-normalize-minimap (or minimap-or-name
                                                 demap-minimap-defalt-name ))))
         (buffer) )
    (when minimap
      (setq buffer (demap-minimap-buffer minimap))
      (cl-dolist (window (get-buffer-window-list buffer nil frame) nil)
        (when (or (window-parameter window 'window-side)
                  (let ((w-parent (window-parent window)))
                    (when w-parent
                      (window-parameter w-parent 'window-side))))
          (delete-window window)
          (cl-return t) )))))

;;;###autoload
(defun demap-minimap-toggle(&optional minimap-or-name frame)
  ""
  (interactive)
  (unless (demap-minimap-close minimap-or-name frame)
    (demap-minimap-open minimap-or-name frame)))



(defun demap-set-open-options(&optional side width)
  ""
  (setq side  (or side 'right)
        width (or width 20) )
  (let ((func (lambda(buffer act)
                (ignore act)
                (demap-buffer-minimap buffer) )))
    (setq display-buffer-alist (assq-delete-all func display-buffer-alist))
    (push `(,func
            (display-buffer-in-side-window)
            (side         . ,side)
            (window-width . ,width) )
          display-buffer-alist )))

(provide 'demap)
;;; demap.el ends here
