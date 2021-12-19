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

(defcustom demap-defalt-buffer-name "*Minimap*"
  "The defalt name to use when makeing a new minimap."
  :type 'string
  :group 'demap)

(defvar demap-minimap-change-functions nil
  "Hook to ran when changing what buffer a demap-minimap is showing.
demap has to replace its buffer whenever it changes
what it shows. this hook is applied after
everything has been moved to the new buffer but
before the old one gets killed. the functions
should take one argument (MINIMAP). MINIMAP is the
minimap that is changing.
sense the buffer will be deleted after this hook is
applied, all local functions will only be applied
once.")

(defvar-local demap--current-minimap nil
  "The minimap associated with this buffer.")


;;;tools

(defun demap--window-replace-buffer(buffer-or-name new-buffer-or-name)
  "Replace the buffer in all windows holding BUFFER-OR-NAME with NEW-BUFFER-OR-NAME."
  (dolist (window (get-buffer-window-list buffer-or-name t t))
    (set-window-buffer window new-buffer-or-name t) ))

(defun demap--buffer-steal-name(buffer)
  "Rename BUFFER and return its old name.
BUFFER's new name is undefined."
  (with-current-buffer buffer
    (let ((name (buffer-name)))
      (rename-buffer "-old minimap buffer-" t)
      name )))


(defun demap--remove-hook-local(hook func buffer)
  "Remove the function FUNC from the buffer-local value of HOOK as BUFFER.
see 'remove-hook'"
  (with-current-buffer buffer
    (remove-hook hook func t) ))

(defun demap--add-hook-local(hook func &optional depth buffer)
  ""
  (with-current-buffer buffer
    (add-hook hook func depth t) ))

(defun demap--smart-add-hook(hook func &optional depth local)
  "Add to the value of HOOK the function FUNC and return cleanup function.
returns a function that, when called, removes FUNC from HOOK.
this function excepts no arguments.
for DEPTH and LOCAL see 'add-hook'"
  (add-hook hook func depth local)
  (if local
      (apply-partially 'demap--remove-hook-local hook func (current-buffer))
    (apply-partially 'remove-hook hook func) ))

(defun demap--smart-add-hook-local(hook func &optional depth buffer)
  ""
  (with-current-buffer buffer
    (demap--smart-add-hook hook func depth t) ))


;;;minimap struct

(cl-defstruct (demap-minimap
               (:copier nil)
               (:constructor demap--minimap-construct) )
  (buffer nil
          :type 'buffer
          :documentation "The buffer associated with demap-minimap.
this slot is read only.")
  (-cleanup-func nil
                 :documentation "a function when called, removes the update function from the hook it is in.
this slot is read only"))

(defun demap-normalize-minimap(minimap-or-name)
  "Return demap-minimap specified by MINIMAP-OR-NAME.
MINIMAP-OR-NAME must be a live minimap, a live
minimap-buffer, a string naming a live minimap or
nil which means to return the minimap for the
current buffer."
  (if (demap-minimap-p minimap-or-name)
      minimap-or-name
    (or (demap-buffer-minimap minimap-or-name)
        (error "No such demap-minimap: %s" minimap-or-name) )))

(defun demap-minimap-live-p(minimap)
  "Determin if MINIMAP is live."
  (and (demap-minimap-p minimap)
       (buffer-live-p (demap-minimap-buffer minimap)) ))

(defun demap--minimap-generate-new-name (&optional name)
  ""
  (generate-new-buffer-name (or name demap-defalt-buffer-name)) )


;;minimap buffer

(defun demap-buffer-minimap(&optional buffer-or-name)
  "Return the demap-minimap associated with BUFFER-OR-NAME.
If BUFFER-OR-NAME is not associated with a minimap then it returns nil."
  (buffer-local-value 'demap--current-minimap (window-normalize-buffer buffer-or-name)))

(defun demap--minimap-buffer-construct(&optional name new-show)
  ""
  (let (buffer)
    (setq name (demap--minimap-generate-new-name name))
    (setq buffer (if new-show
                     (make-indirect-buffer new-show name)
                   (generate-new-buffer name) ))
    (with-current-buffer buffer
      (setq-local auto-hscroll-mode nil)
      (setq vertical-scroll-bar nil
            truncate-lines      t
            buffer-read-only    t )
      (buffer-face-set 'demap-font-face)
      buffer)))

(defun demap--minimap-buffer-reconstruct(minimap &optional new-show)
  ""
  (let (new-buffer old-buffer)
    (setq old-buffer (demap-minimap-buffer minimap))
    (setq new-buffer (demap--minimap-buffer-construct (demap--buffer-steal-name old-buffer) new-show) )
    (demap--window-replace-buffer old-buffer new-buffer)
    new-buffer))

(defun demap--minimap-buffer-set(minimap new-buffer)
  ""
  (let ((old-buffer (demap-minimap-buffer minimap)))
    (when old-buffer
      (with-current-buffer old-buffer
        (kill-local-variable 'demap--current-minimap) )))
  (setf (demap-minimap-buffer minimap) new-buffer)
  (with-current-buffer new-buffer
    (setq-local demap--current-minimap minimap) ))


;;minimap showing

(defun demap-minimap-showing(&optional minimap-or-name)
  "Access the buffer that MINIMAP-OR-NAME is showing.
if MINIMAP-OR-NAME is blank or dead, return nil."
  (let ((minimap (demap-normalize-minimap minimap-or-name)))
    (when (demap-minimap-live-p minimap)
      (buffer-base-buffer (demap-minimap-buffer minimap)) )))

(defun demap-minimap-showing-set-unchecked(minimap &optional new-show)
  "Version of ('demap-minimap-showing-set' MINIMAP NEW-SHOW) without type checking."
  (let (new-buffer old-buffer)
    (setq old-buffer (demap-minimap-buffer minimap))
    (setq new-buffer (demap--minimap-buffer-reconstruct minimap new-show))
    (demap--minimap-buffer-set minimap new-buffer)
    (kill-buffer old-buffer)))

(defun demap-minimap-showing-set(minimap-or-name &optional buffer-or-name)
  "Set the buffer that minimap MINIMAP-OR-NAME is showing to BUFFER-OR-NAME.
if BUFFER-OR-NAME is nil, then the buffer will be blank.
this is equivalent to (setf ('demap-minimap-showing' MINIMAP-OR-NAME) BUFFER-OR-NAME)"
  (let ((minimap (demap-normalize-minimap minimap-or-name))
        (new-show (and buffer-or-name (window-normalize-buffer buffer-or-name))) )
    (demap-minimap-showing-set-unchecked minimap new-show) )
  buffer-or-name )

(gv-define-setter demap-minimap-showing(buffer-or-name minimap-or-name)
  `(demap-minimap-showing-set ,minimap-or-name ,buffer-or-name))


;;minimap update

(defun demap-minimap-update-p(minimap)
  "Determin if demap-minimap MINIMAP can fallow the current window."
  (ignore minimap)
  (buffer-file-name (window-buffer)) )

(defun demap-minimap-update(minimap)
  "Update whet window demap-minimap MINIMAP is showing."
  (when (demap-minimap-update-p minimap)
    (setf (demap-minimap-showing minimap) (window-buffer)) ))

(defun demap-minimap-update-construct(minimap)
  ""
  (apply-partially #'demap-minimap-update minimap))


;;minimap fallow

(defun demap--minimap--cleanup-func-call(minimap)
  "Cleanup the hooks MINIMAP use to update.
see 'demap--minimap-update-hook-set'"
  (let ((fun (demap-minimap--cleanup-func minimap)))
    (when fun
      (funcall fun) )))

(defun demap-minimap-update-hook-set(minimap-or-name &optional hook depth local)
  "Add MINIMAP-OR-NAME's update function to HOOK and remember it.
when this is set, the update funtion is removed
from the old hook and placed in the new one. if
HOOK is nil then it only removes the update
function.
for DEPTH and LOCAL are passed to 'add-hook'."
  (let (minimap update-func claen-func)
    (setf minimap     (demap-normalize-minimap minimap-or-name)
          update-func (demap-minimap-update-construct minimap)
          claen-func  (demap--smart-add-hook hook update-func depth local))
    (demap--minimap--cleanup-func-call minimap)
    (setf (demap-minimap--cleanup-func minimap) claen-func) ))


;;minimap construct

(defun demap-minimap-construct(&optional name)
  "Construct a demap-minimap with name NAME."
  (let ((minimap (demap--minimap-construct)))
    (demap--minimap-buffer-set minimap (demap--minimap-buffer-construct name nil))
    (demap-minimap-update-hook-set minimap 'window-state-change-hook)
    minimap ))



(provide 'demap)
;;; demap.el ends here
