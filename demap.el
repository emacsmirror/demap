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


;;--dependentys
(eval-when-compile
  (require 'cl-lib)
  (when (>= emacs-major-version 28)
    ;window.el dosnt provide 'window before version 28
    (require 'window) ))



;;minimap object

(defvar-local demap--current-minimap nil
  "The minimap asoshiated with this buffer.")

(defvar demap-minimap-change-functions nil
  "Hook to ran when changeing what buffer a demap-minimap is showing.
demap has to replace its buffer whenever it changes
what it shadows. this hook is appliad after
everything has been moved to the new buffer but
before the old one gets killed. the functions should
take one argument (MINIMAP). MINIMAP is the minimap
that is changeing.")


(defun demap-minimap-p(minimap)
  "Determin if MINIMAP is a demap-minimap."
  (and (listp minimap)
       (eq (car minimap) 'demap-minimap) ))

(defun demap-minimap-buffer(minimap)
  "Return the buffer used by MINIMAP.
this buffer can get killed when the minimap
switches what buffer it is shadowing."
  (nth 1 minimap))
(gv-define-setter demap-minimap-buffer(buffer-or-name minimap)
  `(let ((mm ,minimap)
         (bfr (window-normalize-buffer ,buffer-or-name)))
     (when (demap-minimap-buffer mm)
       (with-current-buffer (demap-minimap-buffer mm)
         (kill-local-variable demap--current-minimap) ))
     (with-current-buffer bfr
       (buffer-local-value 'demap--current-minimap bfr) )
     (setf (nth 1 mm) bfr)))

(defun demap-buffer-minimap(&optional buffer-or-name)
  "Return the demap-minimap asoshiated with BUFFER-OR-NAME.
If BUFFER-OR-NAME is not asosiated with a minimap then it returns nil."
  (buffer-local-value 'demap--current-minimap (window-normalize-buffer buffer-or-name)))

(defun demap-minimap-live-p(minimap)
  "Determin if MINIMAP is live."
  (and (demap-minimap-p minimap)
       (buffer-live-p (demap-minimap-buffer minimap)) ))

(defun demap-minimap-showing(minimap)
  "Return the buffer that MINIMAP is showing.
if MINIMAP is blank or dead, return nil."
  (when (demap-minimap-live-p minimap)
    (buffer-base-buffer (demap-minimap-buffer minimap)) ))
(gv-define-setter demap-minimap-showing(buffer-or-name minimap)
  `(let ((mmap ,minimap) (new-show ,buffer-or-name))
     (cl-assert (demap-minimap-p mmap) t "Wrong type argument: demap-minimap, %s")
     (cl-assert (or (not new-show)
                    (buffer-live-p (get-buffer new-show)) )
                t "Wrong type argument: stringp, %s" )
     (with-current-buffer (demap-minimap-buffer mmap)
       (let ((name (buffer-name)))
         (rename-buffer "-old minimap buffer-" t)
         (setf (demap-minimap-buffer mmap) (if new-show
                                             (make-indirect-buffer new-show name)
                                           (generate-new-buffer name) ))
         (dolist (window (get-buffer-window-list nil t t))
           (set-window-buffer window (demap-minimap-buffer mmap) t) )
         (unwind-protect
             (run-hook-with-args demap-minimap-change-functions mmap)
           (kill-buffer) )))))


(defun demap-generate-minimap(name)
  "Genorate and return a new minimap with name NAME."
  (let ((new-map `(demap-minimap nil)))
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
