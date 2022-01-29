;;; demap-minimap.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://github.com/sawyer>
;; Maintainer: Sawyer Gardner <sawyerjgardner@gmail.com>
;; Created: January 03, 2022
;; Modified: January 03, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/sawyer/demap-minimap
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

;;variables

(defface demap-minimap-font-face
  '((default
      :family  "DejaVu Sans Mono"
      :height  30))
  "Face used for demap-minimap buffers."
  :group 'demap)

(defcustom demap-minimap-defalt-name "*Minimap*"
  "The defalt name for new demap-minimaps."
  :group 'demap
  :type 'string)

(defcustom demap-minimap-construct-hook '(demap-track-w-mode demap-current-line-mode demap-visible-region-mode)
  "Normal hook ran after construct a demap-minimap.
this hook is ran has the buffer used by the new minimap."
  :group 'demap
  :type  'hook )

(defcustom demap-minimap-change-functions nil
  "Hook ran when changing a demap-minimap's buffer.
when a demap-minimap needs to rebuild its buffer,
it will copy all protected variables to the new
buffer then run this hook has the old buffer.
the functions in this hook should take one argument
\(MINIMAP). MINIMAP is the minimap that is changing."
  :group 'demap
  :type  'hook )

(defcustom demap-minimap-kill-hook nil
  "Normal hook ran when killing a demap-minimap.
note that demap-minimap sometimes needs to rebuild
its buffer. when this happens, buffer-kill-hook
gets called but not this hook."
  :group 'demap
  :type  'hook )

(defcustom demap-minimap-window-set-hook nil
  "Normal hook ran when demap-minimap sets the window it is showing.
the window demap-minimap is set to show can be got
from `demap-current-minimap-window'."
  :group 'demap
  :type  'hook )

(defcustom demap-minimap-window-sleep-hook nil
  "Normal hook ran when demap-minimap rests.
this is called when the window demap-minimap is
showing is no longer active."
  :group 'demap
  :type  'hook )

(defcustom demap-minimap-protected-variables nil
  "List of variables copy when demap-minimap rebuilds its buffer.
the buffer-local values of these variables are copied from the old buffer to the
new one when demap-minimap rebuilds its buffer. use
`demap-minimap-protect-variables' and `demap-minimap-unprotect-variables' to add
and remove variables from this list."
  :group 'demap
  :type  '(repeat variable) )


(defvar-local demap--current-minimap nil
  "The minimap associated with this buffer.
see `demap-buffer-minimap'.")

(defvar-local demap--minimap-window nil
  "the window that the current minimap is showing.
see `demap-current-minimap-window'.")

;;;minimap struct

(cl-defstruct (demap-minimap
               (:copier nil)
               (:constructor demap--minimap-construct) )
  (buffer nil
          :type 'buffer
          :documentation "The buffer associated with demap-minimap.
this slot is read only."))

(defun demap-normalize-minimap(minimap-or-name)
  "Return the demap-minimap specified by MINIMAP-OR-NAME.
MINIMAP-OR-NAME must be a live minimap, a live
minimap-buffer, a string naming a live minimap or
nil which means to return the minimap for the
current buffer's minimap."
  (if (demap-minimap-p minimap-or-name)
      minimap-or-name
    (or (demap-buffer-minimap minimap-or-name)
        (error "No such demap-minimap: %s" minimap-or-name) )))

(defun demap-minimap-live-p(minimap)
  "Return t if MINIMAP is a demap-minimap which has not been killed.
Value is nil if MINIMAP is not a demap-minimap or if it has been killed."
  (and (demap-minimap-p minimap)
       (buffer-live-p (demap-minimap-buffer minimap)) ))

;;minimap protect

(defun demap--minimap-protected-copy-variables(new-buffer)
  "Copy the current buffers protected variables to NEW-BUFFER.
the protected variables are the ones listed in
`demap-minimap-protected-variables' along with a
few hardcoded ones."
  (dolist (v (list 'demap-minimap-change-functions
                   'demap-minimap-kill-hook
                   'demap-minimap-window-set-hook
                   'demap-minimap-window-sleep-hook
                   'demap-minimap-protected-variables
                   'demap--minimap-window ))
    (demap--tools-copy-local-variable v nil new-buffer) )
  (demap--tools-dolist-hook (v 'demap-minimap-protected-variables)
    (demap--tools-copy-local-variable v nil new-buffer) ))

(defun demap-minimap-protect-variables(local &rest vars)
  "Add VARS to the list of protected variables.
protected variables are listed in `demap-minimap-protected-variables'.
VARS are not added if already present.

if LOCAL is non-nil, VARS are added to the buffer-local
value of `demap-minimap-protected-variables'. t is
also added to signal to use the buffer-local and
global value. this option is only useful if the current
buffer is a demap-minimap buffer."
  (dolist (v vars)
    (add-hook 'demap-minimap-protected-variables v nil local) ))

(defun demap-minimap-unprotect-variables(local &rest vars)
  "Remove VARS from the list of protected variables.
protected variables are listed in `demap-minimap-protected-variables'.
if VARS are not present then nuthing happens.

if LOCAL is non-nil then VARS are removed from the
buffer-local value of `demap-minimap-protected-variables'.

see `demap-minimap-protect-variables'."
  (dolist (v vars)
    (remove-hook 'demap-minimap-protected-variables v local) ))

;;minimap buffer

(defun demap-buffer-minimap(&optional buffer-or-name)
  "Return the demap-minimap associated with BUFFER-OR-NAME.
If BUFFER-OR-NAME is not associated with a minimap then it returns nil."
  (buffer-local-value 'demap--current-minimap (window-normalize-buffer buffer-or-name)) )

(defun demap--minimap-protect-from-base(minimap)
  "Make it so the buffer MINIMAP is showing can not accidentally kill MINIMAP.
without this, killing the buffer that MINIMAP is showing will kill MINIMAP.
this will make MINIMAP change to a blank buffer instead."
  ;TODO: fix this. can cause problems if the base
  ;buffer is killed in demap-minimap-change-hook
  (when (demap-minimap-showing minimap)
    (demap--tools-add-hook-local 'kill-buffer-hook
                                 (demap--tools-smart-add-hook-local 'kill-buffer-hook
                                                              (apply-partially #'demap-minimap-showing-set minimap nil)
                                                              nil (demap-minimap-showing minimap) )
                                 nil (demap-minimap-buffer minimap) )))

(defun demap--minimap-generate-new-name(&optional name)
  "Return a name not used by any buffer based on NAME.
NAME defalts to `demap-minimap-defalt-name'."
  (generate-new-buffer-name (or name demap-minimap-defalt-name)) )

(defun demap--minimap-buffer-construct(&optional name show)
  "Construct a buffer that demap-minimap can use, using NAME and showing SHOW.
name defalts to `demap-minimap-defalt-name' and show
defalts to a blank buffer."
  (let (buffer)
    (setq name (demap--minimap-generate-new-name name))
    (setq buffer (if show
                     (make-indirect-buffer show name)
                   (generate-new-buffer name) ))
    (with-current-buffer buffer
      (setq-local auto-hscroll-mode nil)
      (setq vertical-scroll-bar nil
            truncate-lines      t
            buffer-read-only    t )
      (buffer-face-set 'demap-minimap-font-face)
      buffer )))

(defun demap--minimap-buffer-reconstruct(minimap &optional new-show)
  "Reconstruct the buffer used by MINIMAP but have it show NEW-SHOW.
this will take the buffers name and any window that buffer was in.
the new buffer will be returned but not assigned to MINIMAP."
  (let (old-buffer new-buffer)
    (setq old-buffer (demap-minimap-buffer minimap))
    (setq new-buffer (demap--minimap-buffer-construct (demap--tools-buffer-steal-name old-buffer) new-show) )
    (demap--tools-window-replace-buffer old-buffer new-buffer)
    new-buffer ))

(defun demap--minimap-kill-hook-run()
  "Run hook `demap-minimap-kill-hook'."
  (run-hooks 'demap-minimap-kill-hook))

(defun demap--minimap-buffer-set(minimap new-buffer)
  "Set the buffer used by MINIMAP to NEW-BUFFER.
if MINIMAP already had a buffer, then the old one
is killed."
  (let ((old-buffer (demap-minimap-buffer minimap)))
    (setf (demap-minimap-buffer minimap) new-buffer)
    (with-current-buffer new-buffer
      (setq-local demap--current-minimap minimap)
      (add-hook 'kill-buffer-hook #'demap--minimap-kill-hook-run nil t) )
    (when old-buffer
      (with-current-buffer old-buffer
        (demap--minimap-protected-copy-variables new-buffer)
        (kill-local-variable 'demap--current-minimap)
        (remove-hook 'kill-buffer-hook #'demap--minimap-kill-hook-run t)
        (with-demoted-errors "error in demap-minimap-change-functions: %s"
          (run-hook-with-args 'demap-minimap-change-functions minimap) )
        (kill-buffer old-buffer) ))
    (demap--minimap-protect-from-base minimap) ))

;;minimap showing

(defun demap-minimap-showing(&optional minimap-or-name)
  "Access the buffer that MINIMAP-OR-NAME is showing.
if MINIMAP-OR-NAME is blank or dead, return nil.
note that a demap-minimap can have a blank buffer
without being dead, don't rely on live minimaps
returning a buffer.

this value can be set with setf but it is preferred
to set `demap-minimap-window' instead when appropriate."
  (let ((minimap (demap-normalize-minimap minimap-or-name)))
    (when (demap-minimap-live-p minimap)
      (buffer-base-buffer (demap-minimap-buffer minimap)) )))

(defun demap-minimap-showing-set-unchecked(minimap &optional buffer)
  "Set the buffer that MINIMAP is showing to BUFFER.
Version of (`demap-minimap-showing-set' MINIMAP BUFFER)
but without checking argument types or if buffer
is already being shown.

if BUFFER is nil, then MINIMAP will show a blank
buffer."
  (let ((new-buffer (demap--minimap-buffer-reconstruct minimap buffer)))
    (demap--minimap-buffer-set minimap new-buffer) ))

(defun demap-minimap-showing-set(minimap-or-name &optional buffer-or-name)
  "Set the buffer that minimap MINIMAP-OR-NAME is showing to BUFFER-OR-NAME.
if BUFFER-OR-NAME is nil, then minimap-or-name will
show a blank buffer. if BUFFER-OR-NAME is already
being shown, nuthing happens.
this is equivalent to
\(setf (`demap-minimap-showing' MINIMAP-OR-NAME) BUFFER-OR-NAME)"
  (let ((minimap (demap-normalize-minimap minimap-or-name))
        (new-show (and buffer-or-name (window-normalize-buffer buffer-or-name))) )
    (when (and new-show (buffer-base-buffer new-show))
      (setq new-show (buffer-base-buffer new-show)) )
    (unless (eq (demap-minimap-showing minimap) new-show)
      (demap-minimap-showing-set-unchecked minimap new-show) ))
  buffer-or-name )

(gv-define-setter demap-minimap-showing(buffer-or-name minimap-or-name)
  `(demap-minimap-showing-set ,minimap-or-name ,buffer-or-name))

;;minimap construct

;;;###autoload
(defun demap-minimap-construct(&optional name showing)
  "Construct a demap-minimap.
NAME defalts to `demap-minimap-defalt-name'.
SHOWING defalts to a blank buffer."
  (interactive)
  (let ((minimap (demap--minimap-construct))
        (buffer  (demap--minimap-buffer-construct name showing)) )
    (demap--minimap-buffer-set minimap buffer)
    (with-current-buffer buffer
      (with-demoted-errors "error in demap-minimap-construct-hook: %s"
        (run-hooks 'demap-minimap-construct-hook) ))
    (when (called-interactively-p 'any)
      (message "constructed demap minimap %s" minimap) )
    minimap ))

;;minimap window current

(defun demap-current-minimap-window()
  "The window the current demap-minimap is showing.
demap-minimap is not guaranteed to be showing the
buffer in this window.

you can use setf to set this value. setting this
value changes what buffer the current minimap is
showing to the buffer in window. setting this to
nil will make minimap show a blank buffer.

current buffer might be killed after setting this."
  demap--minimap-window )

(defun demap-current-minimap-window-set(window)
  "Set the window the current demap-minimap is showing to WINDOW.
setting this value changes what buffer the current
minimap is showing to the buffer in window. setting
this to nil will make minimap show a blank buffer.

current buffer might be killed after setting this.

this is the same has
\(setf (demap-current-minimap-window) WINDOW)."
  (let ((minimap (demap-buffer-minimap)))
    (setf demap--minimap-window window
          (demap-minimap-showing (demap-buffer-minimap)) (window-buffer window) )
    (with-demoted-errors "error in demap-minimap-window-set-hook: %s"
      ;minimap might have changed buffer by now
      (with-current-buffer (demap-minimap-buffer minimap)
        (run-hooks 'demap-minimap-window-set-hook) ))))

(gv-define-simple-setter demap-current-minimap-window demap-current-minimap-window-set)

;;minimap window

(defun demap-minimap-window(&optional minimap-or-name)
  "The window that MINIMAP-OR-NAME is showing.
MINIMAP-OR-NAME is not guaranteed to be showing the
buffer in this window.

you can use setf to set this value. setting this
value changes what buffer MINIMAP-OR-NAME is
showing to the buffer in window. setting this to
nil will make MINIMAP-OR-NAME show a blank buffer."
  (with-current-buffer (demap-minimap-buffer (demap-normalize-minimap minimap-or-name))
    (demap-current-minimap-window) ))

(defun demap-minimap-window-set(&optional minimap-or-name window)
  "Set the window MINIMAP-OR-NAME is showing to WINDOW.
setting this value changes what buffer MINIMAP-OR-NAME
is showing to the buffer in window. setting this to
nil will make MINIMAP-OR-NAME show a blank buffer.

this is the same has
\(setf (demap-minimap-window MINIMAP-OR-NAME) WINDOW)."
  (with-current-buffer (demap-minimap-buffer (demap-normalize-minimap minimap-or-name))
    (demap-current-minimap-window-set window) ))

(gv-define-setter demap--minimap-window-as(window minimap-or-name)
  `(demap--minimap-window-set-as ,minimap-or-name ,window))


(defun demap-current-minimap-window-sleep()
  "Run hook `demap-minimap-window-sleep-hook'."
  (with-demoted-errors "error in demap-minimap-window-sleep-hook: %s"
    (run-hooks 'demap-minimap-window-sleep-hook) ))

(defun demap-minimap-window-sleep(minimap)
  "Run hook `demap-minimap-window-sleep-hook' has MINIMAP."
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap-current-minimap-window-sleep) ))



(provide 'demap-minimap)
(require 'demap)
;(provide 'demap)
;;; demap-minimap.el ends here
