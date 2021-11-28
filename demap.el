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
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;--dependents
(eval-when-compile
  (require 'cl-lib)
  (when (>= emacs-major-version 28)
    ;window.el doesn't provide 'window before version 28
    (require 'window) ))


(defgroup demap nil
  "A detachable minimap for Emacs."
  :group 'convenience)

(defface demap-font-face
  '((default :family "DejaVu Sans Mono" :height 30))
  "Face used for the body of the minimap."
  :group 'demap)

(defface demap-visable-region-face
  '((((background dark)) (:background "#700000" :extend t))
    (t (:background "#C847D8FEFFFF" :extend t)))
  "Face used to represent the part of the minimap visible throw the main window."
  :group 'demap)

(defface demap-current-line-face
  '((((background dark)) (:background "dark gray"))
    (t (:background "dark gray")))
  "Face used to show the current line."
  :group 'demap)


(defvar demap-minimap-change-functions nil
  "Hook to ran when changing what buffer a demap-minimap is showing.
demap has to replace its buffer whenever it changes
what it shadows. this hook is applied after
everything has been moved to the new buffer but
before the old one gets killed. the functions should
take one argument (MINIMAP). MINIMAP is the minimap
that is changing.")

(defvar-local demap--current-minimap nil
  "The minimap associated with this buffer.")


(defun demap--window-replace-buffer(buffer-or-name new-buffer-or-name)
  "Replace the buffer in all windows holding BUFFER-OR-NAME with NEW-BUFFER-OR-NAME."
  (dolist (window (get-buffer-window-list buffer-or-name t t))
    (set-window-buffer window new-buffer-or-name t) ))

(defun demap--buffer-steal-name(&optional buffer)
  "Rename BUFFER and return its old name.
BUFFER's new name is undefined."
  (with-current-buffer buffer
    (let ((name (buffer-name)))
      (rename-buffer "-old minimap buffer-" t)
      name )))


;;minimap buffer

(defun demap--generate-buffer(name &optional buffer-show)
  "Make a new buffer for demap-minimap with name NAME and showing BUFFER-SHOW."
  (if buffer-show
      (make-indirect-buffer buffer-show name)
    (generate-new-buffer name) ))

(defun demap--generate-minimap-buffer(name &optional buffer-show)
  "Make and setup a buffer for demap-minimap with name NAME and showing BUFFER-SHOW."
  (with-current-buffer (demap--generate-buffer name buffer-show)
    (buffer-face-set 'demap-font-face)
    (current-buffer) ))

(defun demap--remake-minimap-buffer(old-buffer-or-name buffer-show)
  "Make a copy of OLD-BUFFER-OR-NAME but have it show BUFFER-SHOW."
  (demap--generate-minimap-buffer (demap--buffer-steal-name old-buffer-or-name) buffer-show) )

(defun demap-buffer-minimap(&optional buffer-or-name)
  "Return the demap-minimap associated with BUFFER-OR-NAME.
If BUFFER-OR-NAME is not associated with a minimap then it returns nil."
  (buffer-local-value 'demap--current-minimap (window-normalize-buffer buffer-or-name)))

(defun demap--run-minimap-change-functions(buffer minimap)
  "Run hook 'demap-minimap-change-functions' has BUFFER, with MINIMAP."
  (with-current-buffer buffer
    (run-hook-with-args demap-minimap-change-functions minimap) ))

(defun demap--kill-old-minimap-buffer(minimap-buffer minimap)
  "Kill buffer MINIMAP-BUFFER that used to be associated with MINIMAP."
  (unwind-protect
      (demap--run-minimap-change-functions minimap-buffer minimap)
    (kill-buffer minimap-buffer) ))


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

(defun demap--minimap-buffer-set(minimap buffer-or-name)
  "Set the buffer used by minimap MINIMAP to BUFFER-OR-NAME.
identical to (setf ('demap-minimap-buffer' MINIMAP) BUFFER-OR-NAME)"
  (let ((bfr (window-normalize-buffer buffer-or-name)))
    (when (demap-minimap-buffer minimap)
      (with-current-buffer (demap-minimap-buffer minimap)
        (kill-local-variable 'demap--current-minimap) ))
    (with-current-buffer bfr
      (setq demap--current-minimap minimap) )
    (setf (nth 1 minimap) bfr)))

(gv-define-setter demap-minimap-buffer(buffer-or-name minimap)
  `(demap--minimap-buffer-set ,minimap ,buffer-or-name))

(defun demap-minimap-live-p(minimap)
  "Determin if MINIMAP is live."
  (and (demap-minimap-p minimap)
       (buffer-live-p (demap-minimap-buffer minimap)) ))

(defun demap--minimap-swapout-buffer(minimap minimap-buffer)
  "Replace the buffer in minimap MINIMAP with MINIMAP-BUFFER."
  (demap--window-replace-buffer (demap-minimap-buffer minimap) minimap-buffer)
  (setf (demap-minimap-buffer minimap) minimap-buffer) )

(defun demap--unsafe-minimap-showing-set(minimap new-show)
  ""
  (let ((old-minimap-buffer (demap-minimap-buffer minimap)))
    (demap--minimap-swapout-buffer minimap (demap--remake-minimap-buffer old-minimap-buffer new-show))
    (demap--kill-old-minimap-buffer old-minimap-buffer minimap) ))

(defun demap-minimap-showing(minimap)
  "Return the buffer that MINIMAP is showing.
if MINIMAP is blank or dead, return nil."
  (when (demap-minimap-live-p minimap)
    (buffer-base-buffer (demap-minimap-buffer minimap)) ))

(defun demap-minimap-showing-set(minimap buffer-or-name)
  "Set the buffer that minimap MINIMAP is showing to BUFFER-OR-NAME.
this is equivalent to (setf ('demap-minimap-showing' MINIMAP) BUFFER-OR-NAME)"
  (cl-assert (demap-minimap-p minimap) t "Wrong type argument: demap-minimap, %s")
  (cl-assert (or (not buffer-or-name)
                 (buffer-live-p (get-buffer buffer-or-name)) )
                 t "Wrong type argument: stringp, %s" )
  (demap--unsafe-minimap-showing-set minimap buffer-or-name)
  buffer-or-name )

(gv-define-setter demap-minimap-showing(buffer-or-name minimap)
  `(demap-minimap-showing-set ,minimap ,buffer-or-name))


(defun demap-generate-minimap(name)
  "Genorate and return a new minimap with name NAME."
  (let ((new-map `(demap-minimap nil)))
    (setf (demap-minimap-buffer new-map) (demap--generate-minimap-buffer name))
    new-map ))

(defun demap-kill-minimap(minimap)
  "Distroy the minimap buffer MINIMAP."
  (cl-assert (demap-minimap-p minimap) t "Wrong type argument: demap-minimap, %s")
  (when (demap-minimap-live-p minimap)
    (kill-buffer (demap-minimap-buffer minimap)) ))


(provide 'demap)
;;; demap.el ends here
