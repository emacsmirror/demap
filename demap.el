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


;;;###autoload
(defun demap-test()
  ""
  (x-focus-frame nil)
  (let ((window  (split-window-right))
        (minimap (demap-minimap-construct)) )
    (set-window-buffer window (demap-minimap-buffer minimap))))

(provide 'demap)
;;; demap.el ends here
