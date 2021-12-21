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

(defface demap-visible-region-face
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
  "The defalt name to use when making a new minimap."
  :type 'string
  :group 'demap)

(defvar demap-minimap-change-functions nil
  "Hook ran when changing what buffer a demap-minimap is showing.
demap has to replace its buffer whenever it changes
what it shows. this hook is applied after
everything has been moved to the new buffer but
before the old one gets killed. the functions
should take one argument (MINIMAP). MINIMAP is the
minimap that is changing.
sense the buffer will be deleted after this hook is
applied, all buffer-local functions will only be applied
once.
for a hook that is not reset every change,
see `demap-minimap-persistant-change-functions'.")

(defvar demap-minimap-persistant-change-functions nil
  "Hook ran when changing what buffer a demap-minimap is showing.
the functions should take one argument (MINIMAP).
MINIMAP is the minimap that is changing.
unlike `demap-minimap-change-functions', all
buffer-local functions get moved to the new buffer
after they are all ran.")

(defvar demap-minimap-kill-hook nil
  "Normal hook ran when killing a minimap.
this is called from within `kill-buffer-hook' in
the minimap's buffer. unlike
`demap-minimap-change-functions', all buffer-local
functions get moved to the new buffer when the
minimap changes what it is showing.
the minimap's buffer is current when the hook
functions run. use `demap-buffer-minimap' to get
the minimap being killed.")


(defvar-local demap--current-minimap nil
  "The minimap associated with this buffer.")


;;;tools

(defun demap--window-replace-buffer(buffer-or-name new-buffer-or-name)
  "Replace the buffer in all windows holding BUFFER-OR-NAME with NEW-BUFFER-OR-NAME."
  (dolist (window (get-buffer-window-list buffer-or-name t t))
    (set-window-buffer window new-buffer-or-name t) ))

(defun demap--buffer-steal-name(buffer-or-name)
  "Rename BUFFER-OR-NAME and return its old name.
BUFFER-OR-NAME's new name is undefined."
  (with-current-buffer buffer-or-name
    (let ((name (buffer-name)))
      (rename-buffer "-old minimap buffer-" t)
      name )))

(defun demap--copy-local-variable(variable from-buffer to-buffer)
  "Copy the buffer-local value of VARIABLE in FROM-BUFFER to TO-BUFFER.
if VARABLE is not buffer local in FROM-BUFFER, then
it will no longer be buffer local in TO-BUFFER."
  (if (local-variable-p variable from-buffer)
      (setf (buffer-local-value variable to-buffer) (buffer-local-value variable from-buffer))
    (with-current-buffer to-buffer
      (kill-local-variable variable) )))


(defun demap--remove-hook-local(hook func &optional buffer)
  "Remove the function FUNC from the buffer-local value of HOOK as BUFFER.
see `remove-hook'."
  (with-current-buffer buffer
    (remove-hook hook func t) ))

(defun demap--add-hook-local(hook func &optional depth buffer)
  "Add the function FUNC to the buffer-local value of HOOK as BUFFER.
see `add-hook'."
  (with-current-buffer buffer
    (add-hook hook func depth t) ))

(defun demap--smart-add-hook(hook func &optional depth local)
  "Add to the value of HOOK the function FUNC and return a cleanup function.
returns a function that, when called, removes FUNC from HOOK.
this function excepts no arguments.
for DEPTH and LOCAL, see `add-hook'."
  (add-hook hook func depth local)
  (if local
      (apply-partially 'demap--remove-hook-local hook func (current-buffer))
    (apply-partially 'remove-hook hook func) ))

(defun demap--smart-add-hook-local(hook func &optional depth buffer)
  "Add the function FUNC to the buffer-local value of HOOK as BUFFER.
returns a function that removes FUNC from HOOK when called.
for DEPTH, see `add-hook'."
  (with-current-buffer buffer
    (demap--smart-add-hook hook func depth t) ))


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

(defun demap--minimap-generate-new-name (&optional name)
  "Return a name not used by any buffer based on NAME.
defalt to `demap-defalt-buffer-name'."
  (generate-new-buffer-name (or name demap-defalt-buffer-name)) )


;;minimap buffer

(defun demap-buffer-minimap(&optional buffer-or-name)
  "Return the demap-minimap associated with BUFFER-OR-NAME.
If BUFFER-OR-NAME is not associated with a minimap then it returns nil."
  (buffer-local-value 'demap--current-minimap (window-normalize-buffer buffer-or-name)))


(defun demap--minimap-protect-from-base(minimap)
  "Make it so the buffer MINIMAP is showing can not accidentally kill MINIMAP.
without this, killing the buffer that MINIMAP is showing will kill MINIMAP.
this will make MINIMAP change to a blank buffer instead."
  (when (demap-minimap-showing minimap)
    (demap--add-hook-local 'kill-buffer-hook
                           (demap--smart-add-hook-local 'kill-buffer-hook
                                                        (apply-partially #'demap-minimap-showing-set minimap nil)
                                                        nil (demap-minimap-showing minimap) )
                           nil (demap-minimap-buffer minimap) )))

(defun demap--minimap-buffer-construct(&optional name show)
  "Construct a buffer that demap-minimap can use, using NAME and showing SHOW.
name defalts to `demap-defalt-buffer-name' and show
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
      (buffer-face-set 'demap-font-face)
      buffer)))

(defun demap--minimap-buffer-reconstruct(minimap &optional new-show)
  "Reconstruct the buffer used by MINIMAP but have it show NEW-SHOW.
this will take the buffers name and any window that buffer was in.
the new buffer will be returned but not assigned to MINIMAP."
  (let (new-buffer old-buffer)
    (setq old-buffer (demap-minimap-buffer minimap))
    (setq new-buffer (demap--minimap-buffer-construct (demap--buffer-steal-name old-buffer) new-show) )
    (demap--window-replace-buffer old-buffer new-buffer)
    new-buffer))

(defun demap--minimap-kill-hook-run()
  "Run hook `demap-minimap-kill-hook'."
  (run-hooks 'demap-minimap-kill-hook))

(defun demap--minimap-change-functions-run(minimap)
  "Run minimap change function hook with argument MINIMAP.
runs hooks `demap-minimap-change-functions' and
`demap-minimap-persistant-change-functions'. also
copy `demap-minimap-persistant-change-functions'
local value to the minimap's buffer."
  (with-demoted-errors "error in demap-minimap-change-functions: %s"
    (run-hook-with-args 'demap-minimap-change-functions minimap) )
  (with-demoted-errors "error in demap-minimap-persistant-change-functions: %s"
    (run-hook-with-args 'demap-minimap-persistant-change-functions minimap) )
  (demap--copy-local-variable 'demap-minimap-persistant-change-functions (current-buffer) (demap-minimap-buffer minimap)) )

(defun demap--minimap-buffer-set(minimap new-buffer)
  "Set the buffer used by MINIMAP to NEW-BUFFER.
the old buffer will be killed."
  (let ((old-buffer (demap-minimap-buffer minimap)))
    (setf (demap-minimap-buffer minimap) new-buffer)
    (with-current-buffer new-buffer
      (setq-local demap--current-minimap minimap)
      (add-hook 'kill-buffer-hook #'demap--minimap-kill-hook-run nil t) )
    (when old-buffer
      (with-current-buffer old-buffer
        (kill-local-variable 'demap--current-minimap)
        (remove-hook 'kill-buffer-hook #'demap--minimap-kill-hook-run t)
        (demap--copy-local-variable 'demap-minimap-kill-hook old-buffer new-buffer)
        (demap--minimap-change-functions-run minimap))
      (kill-buffer old-buffer) )
    (demap--minimap-protect-from-base minimap) ))


;;minimap showing

(defun demap-minimap-showing(&optional minimap-or-name)
  "Access the buffer that MINIMAP-OR-NAME is showing.
if MINIMAP-OR-NAME is blank or dead, return nil.
note that a demap-minimap can have a blank buffer
without being dead, don't rely on live minimaps
returning a buffer."
  (let ((minimap (demap-normalize-minimap minimap-or-name)))
    (when (demap-minimap-live-p minimap)
      (buffer-base-buffer (demap-minimap-buffer minimap)) )))

(defun demap-minimap-showing-set-unchecked(minimap &optional buffer)
  "Set the buffer that MINIMAP is showing to BUFFER.
Version of (`demap-minimap-showing-set' MINIMAP BUFFER)
but without checking argument types or if buffer
is already being shown.
if BUFFER is nil, then the buffer will be blank."
  (let ((new-buffer (demap--minimap-buffer-reconstruct minimap buffer)))
    (demap--minimap-buffer-set minimap new-buffer) ))

(defun demap-minimap-showing-set(minimap-or-name &optional buffer-or-name)
  "Set the buffer that minimap MINIMAP-OR-NAME is showing to BUFFER-OR-NAME.
if BUFFER-OR-NAME is nil, then the buffer will be
blank. if BUFFER-OR-NAME is already being shown, do
nuthing.
this is equivalent to (setf (`demap-minimap-showing' MINIMAP-OR-NAME) BUFFER-OR-NAME)"
  (let ((minimap (demap-normalize-minimap minimap-or-name))
        (new-show (and buffer-or-name (window-normalize-buffer buffer-or-name))) )
    (when (and new-show (buffer-base-buffer new-show))
      (setq new-show (buffer-base-buffer new-show)) )
    (unless (eq (demap-minimap-showing minimap) new-show)
      (demap-minimap-showing-set-unchecked minimap new-show) ))
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


(defun demap-minimap-smart-add-hook-update(minimap-or-name hook &optional depth local)
  "Add MINIMAP-OR-NAME's update function to HOOK and return cleanup function.
adds a function that updates MINIMAP-OR-NAME to a
normal hook HOOK and removes that function
automatically when MINIMAP-OR-NAME is killed. this
also returns a function that when called, removes
the update function from HOOK now instead of later.
DEPTH and LOCAL are passed to `add-hook'."
  (let ((minimap (demap-normalize-minimap minimap-or-name))
        (update-func)
        (clean-func) )
    (setq update-func (apply-partially #'demap-minimap-update minimap)
          clean-func  (demap--smart-add-hook hook update-func depth local) )
    (demap--add-hook-local 'demap-minimap-kill-hook clean-func nil (demap-minimap-buffer minimap))
    (lambda()
      (demap--remove-hook-local 'demap-minimap-kill-hook clean-func (demap-minimap-buffer minimap))
      (funcall clean-func) )))

;;minimap construct

(defun demap-minimap-construct(&optional name)
  "Construct a demap-minimap with name NAME."
  (let ((minimap (demap--minimap-construct)))
    (demap--minimap-buffer-set minimap (demap--minimap-buffer-construct name nil))
    (demap-minimap-smart-add-hook-update minimap 'window-state-change-hook)
    minimap ))



(provide 'demap)
;;; demap.el ends here
