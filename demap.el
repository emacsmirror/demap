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
  '((((background dark)) (:background "gray10" :extend t))
    (t (:background "gray10" :extend t)))
  "Face used to for the part of the minimap visible throw the main window."
  :group 'demap)

(defface demap-current-line-face
  '((((background dark)) (:background "gray50" :extend t))
    (t (:background "gray50" :extend t)))
  "Face used to show the current line."
  :group 'demap)

(defface demap-visible-region-inactive-face
  '((((background dark)) (:background "gray16" :extend t))
    (t (:background "gray16" :extend t)))
  "Face used to represent the part of the minimap visible throw the main window."
  :group 'demap)

(defface demap-current-line-inactive-face
  '((((background dark)) (:background "gray77" :extend t))
    (t (:background "gray77" :extend t)))
  "Face used to show the current line."
  :group 'demap)


(defcustom demap-defalt-buffer-name "*Minimap*"
  "The defalt name to use when making a new minimap."
  :type 'string
  :group 'demap)

(defvar demap-minimap-change-once-functions nil
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
see `demap-minimap-change-functions'.")

(defvar demap-minimap-change-functions nil
  "Hook ran when changing what buffer a demap-minimap is showing.
the functions should take one argument (MINIMAP).
MINIMAP is the minimap that is changing.
unlike `demap-minimap-change-once-functions', all
buffer-local functions get moved to the new buffer
after they are all ran.")

(defvar demap-minimap-kill-hook nil
  "Normal hook ran when killing a minimap.
this is called from within `kill-buffer-hook' in
the minimap's buffer. unlike
`demap-minimap-change-once-functions', all buffer-local
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

(defun demap--real-buffer(buffer)
  "Return base buffer of BUFFER.
if Buffer is not an indirect buffer, return BUFFER.
see `buffer-base-buffer'."
  (or (buffer-base-buffer buffer) buffer))

(defun demap--copy-local-variable(variable from-buffer to-buffer)
  "Copy the buffer-local value of VARIABLE in FROM-BUFFER to TO-BUFFER.
if VARABLE is not buffer local in FROM-BUFFER, then
it will no longer be buffer local in TO-BUFFER."
  (if (local-variable-p variable from-buffer)
      (setf (buffer-local-value variable to-buffer) (buffer-local-value variable from-buffer))
    (with-current-buffer to-buffer
      (kill-local-variable variable) )))

(defun demap--list-p(obj)
  "Determin if OBJ is a list.
if OBJ is a list and not a lambda or nil, return t,
otherwise nil."
  (and obj
       (listp obj)
       (not (functionp obj)) ))

(defmacro demap--dolist(spec &rest body)
  "Loop over a list or object SPEC.
Evaluate BODY with VAR bound to each car from LIST,
in turn. Then evaluate RESULT to get return value,
default nil. if LIST is not a list then evaluate
BODY with VAR bound to the value of LIST.
see `dolist'.
\(fn (VAR LIST [RESULT]) BODY...)"
  (declare (indent 1))
  (let ((tempvar (make-symbol "tempvar")))
    `(let ((,tempvar ,(nth 1 spec)))
       (if (demap--list-p ,tempvar)
           (dolist (,(nth 0 spec) ,tempvar ,(nth 2 spec))
             ,@body )
         (dolist (,(nth 0 spec) (list ,tempvar) ,(nth 2 spec))
           ,@body )))))

(defmacro demap--dolists-unsafe(specs &rest body)
  "Loop over all SPECS without type checking.
unsafe version of `demap--dolists'.
\(fn ((VAR LIST [STEP])...) BODY...)"
  (declare (indent 1))
  (if specs
      `(demap--dolist ,(car specs)
         (demap--dolists-unsafe ,(cdr specs)
           ,@body ))
    `(progn
       ,@body )))

(defmacro demap--dolists(specs &rest body)
  "Loop over lists or objects in SPECS.
Evaluate BODY with VAR bound to each car from LIST,
in turn. if LIST is not a list then evaluate BODY
with VAR bound to the value of LIST. this process
is stacked for each VAR and LIST given, evaluateing
BODY with every combanation a LIST elements. STEP
is evaluated each time the end of LIST is reached.
returns the value of STEP in the first spec.
see `dolist'.
\(fn ((VAR LIST [STEP])...) BODY...)"
  (unless (listp specs)
    (signal 'wrong-type-argument (list 'consp specs)))
  (unless (<= 1 (length specs))
    (signal 'wrong-number-of-arguments (list 1 (length specs))))
  `(demap--dolists-unsafe ,specs
     ,@body))


(defalias 'demap--add-hook    #'add-hook)
(defalias 'demap--remove-hook #'remove-hook)

(defun demap--add-hooks(hooks funcs &optional depth local)
  "Add to the value of HOOKS the functions FUNCS.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

DEPTH and LOCAL are passed to `add-hook'."
  (demap--dolists ((hook hooks)
                   (func funcs) )
    (demap--add-hook hook func depth local)))

(defun demap--remove-hooks(hooks funcs &optional local)
  "Remove FUNCS from the value of HOOKS.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. if function is not in hook, then it is
skipped.

if LOCAL is non-nil then HOOKS are buffer local.
see `remove-hook'."
  (demap--dolists ((hook hooks)
                   (func funcs) )
    (demap--remove-hook hook func local) ))

(defun demap--smart-add-hook(hook func &optional depth local)
  "Add to the value of HOOK the function FUNC and return a cleanup function.
returns a function that, when called, removes FUNC
from HOOK. the returned function excepts no arguments.

DEPTH and LOCAL are passed to `add-hook'."
  (demap--add-hook hook func depth local)
  (if local
      (apply-partially 'demap--remove-hook-local hook func (current-buffer))
    (apply-partially 'demap--remove-hook hook func) ))

(defun demap--smart-add-hooks(hooks funcs &optional depth local)
  "Add to the values of HOOKS the functions FUNCS and return a cleanup function.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

returns a function that, when called, removes FUNCS
from HOOKS. the returned function excepts no arguments.

DEPTH and LOCAL are passed to `add-hook'."
  (demap--add-hooks hooks funcs depth local)
  (if local
      (apply-partially 'demap--remove-hooks-local hooks funcs (current-buffer))
    (apply-partially 'demap--remove-hooks hooks funcs) ))


(defun demap--add-hook-local(hook func &optional depth buffer)
  "Add the function FUNC to the buffer-local value of HOOK as BUFFER.
see `add-hook'."
  (with-current-buffer buffer
    (demap--add-hook hook func depth t) ))

(defun demap--remove-hook-local(hook func &optional buffer)
  "Remove the functions FUNC from the buffer-local values of HOOK as BUFFER.
see `remove-hook'."
  (with-current-buffer buffer
    (demap--remove-hook hook func t) ))

(defun demap--add-hooks-local(hooks funcs &optional depth buffer)
  "Add the functions FUNCS to the buffer-local values of HOOKS as BUFFER.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

DEPTH is passed to `add-hook'."
  (with-current-buffer buffer
    (demap--add-hooks hooks funcs depth t) ))

(defun demap--remove-hooks-local(hooks funcs &optional buffer)
  "Remove the functions FUNCS from the buffer-local values of HOOKS as BUFFER.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. if function is not in hook, then it is
skipped.

see `remove-hook'."
  (with-current-buffer buffer
    (demap--remove-hooks hooks funcs t) ))

(defun demap--smart-add-hook-local(hook func &optional depth buffer)
  "Add FUNC to the buffer-local value of HOOK and return a cleanup function.
returns a function that, when called, removes FUNC
from HOOK. the returned function excepts no arguments.

DEPTH and LOCAL are passed to `add-hook'."
  (with-current-buffer buffer
    (demap--smart-add-hook hook func depth t) ))

(defun demap--smart-add-hooks-local(hooks funcs &optional depth buffer)
  "Add FUNCS to the buffer-local values of HOOKS and return a cleanup function.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

returns a function that, when called, removes FUNCS
from HOOKS. the returned function excepts no arguments.

DEPTH is passed to `add-hook'."
  (with-current-buffer buffer
    (demap--smart-add-hooks hooks funcs depth t) ))


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

(defun demap-remove-hook-minimap-local(hook func &optional minimap)
  "Remove the function FUNC from the buffer local value of HOOK as MINIMAP's buffer."
  (demap--remove-hooks-local hook func (demap-minimap-buffer minimap)))

(defun demap-smart-add-hook-minimap-local(hook func &optional depth minimap)
  "Add the function FUNC to the buffer-local value of HOOK as MINIMAP's buffer.
returns a function that when called, removes FUNC from HOOK.
only usefall for `demap-minimap-change-functions'
or `demap-minimap-kill-hook'.
for DEPTH, see `add-hook'."
  (with-current-buffer (demap-minimap-buffer minimap)
    (add-hook hook func depth t)
    (apply-partially #'demap-remove-hook-minimap-local hook func minimap)))

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
runs hooks `demap-minimap-change-once-functions' and
`demap-minimap-change-functions'. also
copy `demap-minimap-change-functions'
local value to MINIMAP's buffer."
  (with-demoted-errors "error in demap-minimap-change-functions: %s"
    (run-hook-with-args 'demap-minimap-change-once-functions minimap) )
  (with-demoted-errors "error in demap-minimap-persistant-change-functions: %s"
    (run-hook-with-args 'demap-minimap-change-functions minimap) )
  (demap--copy-local-variable 'demap-minimap-change-functions (current-buffer) (demap-minimap-buffer minimap)) )

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


;;minimap construct

(defun demap-minimap-construct(&optional name showing)
  ""
  (let ((minimap (demap--minimap-construct)))
    (demap--minimap-buffer-set minimap (demap--minimap-buffer-construct name showing))
    minimap))


;;;line-ov-------

(cl-defstruct (demap-line-ov
               (:copier nil)
               (:constructor nil)
               (:constructor demap-line-ov-construct
                (minimap
                 &aux
                 (overlay (make-overlay 0 0 (demap-minimap-buffer minimap))) )))
  (minimap nil :read-only t)
  (overlay nil :read-only t)
  (window  nil) )


;;line-ov update

(defun demap-line-ov-update(line-ov)
  ""
  (if (eq (selected-window) (demap-line-ov-window line-ov))
      (move-overlay (demap-line-ov-overlay line-ov)
                    (line-beginning-position)
                    (+ (line-end-position) 1)
                    (demap-minimap-buffer (demap-line-ov-minimap line-ov)) )
    (demap-line-ov-unset line-ov) ))

;;line-ov set

(defun demap-line-ov-unset(line-ov)
  ""
  (overlay-put (demap-line-ov-overlay line-ov) 'face 'demap-current-line-inactive-face)
  (remove-hook 'post-command-hook (apply-partially #'demap-line-ov-update line-ov)) )

(defun demap-line-ov-set(line-ov window)
  ""
  (when window
    (overlay-put (demap-line-ov-overlay line-ov) 'face 'demap-current-line-face)
    (setf (demap-line-ov-window line-ov) window)
    (add-hook 'post-command-hook (apply-partially #'demap-line-ov-update line-ov))
    (demap-line-ov-update line-ov) ))


;;;area-ov-------

(cl-defstruct (demap-area-ov
               (:copier nil)
               (:constructor nil)
               (:constructor demap-area-ov-construct
                (minimap
                 &aux
                 (overlay (make-overlay 0 0 (demap-minimap-buffer minimap))) )))
  (minimap nil :read-only t)
  (overlay nil :read-only t)
  (window  nil) )


(defun demap-area-ov-active-p(area-ov)
  ""
  (let ((window  (demap-area-ov-window  area-ov))
        (minimap (demap-area-ov-minimap area-ov)) )
    (and (window-live-p window)
         (eq (demap--real-buffer (window-buffer window)) (demap-minimap-showing minimap)) )))

;;area-ov update

(defun demap-area-ov-update(area-ov &optional &rest r)
  ""
  (ignore r)
  (let ((window (demap-area-ov-window area-ov)))
    (if (demap-area-ov-active-p area-ov)
        (move-overlay (demap-area-ov-overlay area-ov)
                      (window-start window)
                      (window-end window t)
                      (demap-minimap-buffer (demap-area-ov-minimap area-ov)) )
      (demap-area-ov-unset area-ov) )))

(defun demap-area-ov-update-if-window(area-ov window &optional &rest r)
  ""
  (ignore r)
  (when (eq (demap-area-ov-window area-ov) window)
    (demap-area-ov-update area-ov) ))

;;area-ov set

(defun demap-area-ov-unset(area-ov)
  ""
  (let ((upd-if-finc (apply-partially #'demap-area-ov-update-if-window area-ov))
        (update-func (apply-partially #'demap-area-ov-update area-ov)) )
    (remove-hook 'window-scroll-functions      upd-if-finc)
    (remove-hook 'window-size-change-functions update-func) ))

(defun demap-area-ov-set(area-ov window)
  ""
  (if (not window)
      (overlay-put (demap-area-ov-overlay area-ov) 'face 'demap-visible-region-inactive-face )
    (overlay-put (demap-area-ov-overlay area-ov) 'face 'demap-visible-region-face)
    (setf (demap-area-ov-window area-ov) window)
    (let ((upd-if-finc (apply-partially #'demap-area-ov-update-if-window area-ov))
          (update-func (apply-partially #'demap-area-ov-update area-ov)) )
      (add-hook 'window-scroll-functions      upd-if-finc)
      (add-hook 'window-size-change-functions update-func) )
    (demap-area-ov-update area-ov) ))


;;;track-w-------

(cl-defstruct (demap-track-w
               (:copier nil)
               (:constructor nil)
               (:constructor demap-track-w-construct-unhooked
                (&optional
                 (minimap (demap-minimap-construct))
                 (line-ov (demap-line-ov-construct minimap))
                 (area-ov (demap-area-ov-construct minimap)) )))
  (minimap nil :read-only t)
  (line-ov nil)
  (area-ov nil) )


(defun demap-track-w-test-window-p(track-w);TODO: redo doc for all in view update
  "Determin if demap-minimap TRACK-W can fallow the current window."
  (ignore track-w)
  (buffer-file-name (window-buffer)) )

;;track-w window

(defun demap-track-w-window-set(track-w window)
  ""
  ;;TODO clean with when-let
  (let ((minimap (demap-track-w-minimap track-w))
        (line-ov (demap-track-w-line-ov track-w))
        (area-ov (demap-track-w-area-ov track-w)) )
    (when window
      (setf (demap-minimap-showing minimap) (window-buffer window)) )
    (when line-ov
      (demap-line-ov-set line-ov window) )
    (when area-ov
      (demap-area-ov-set area-ov window) )))

;;track-w update

(defun demap-track-w-update(track-w)
  "Update whet window demap-minimap TRACK-W is showing."
  (demap-track-w-window-set track-w (when (demap-track-w-test-window-p track-w)
                                      (selected-window) )))

;;track-w hook

(defun demap-track-w-unhook(track-w)
  ""
  (let ((update-func (apply-partially #'demap-track-w-update track-w))
        (unhook-func (apply-partially #'demap-track-w-unhook track-w))
        (buffer  (demap-minimap-buffer (demap-track-w-minimap track-w)))
        (line-ov (demap-track-w-line-ov track-w))
        (area-ov (demap-track-w-area-ov track-w)) )
    (demap--remove-hook       'window-state-change-hook update-func)
    (demap--remove-hook-local 'demap-minimap-kill-hook  unhook-func buffer)
    (when line-ov
      (demap-line-ov-unset line-ov) )
    (when area-ov
      (demap-area-ov-unset area-ov) ) ))

(defun demap-track-w-hook(track-w)
  ""
  (let ((update-func (apply-partially #'demap-track-w-update track-w))
        (unhook-func (apply-partially #'demap-track-w-unhook track-w))
        (buffer      (demap-minimap-buffer (demap-track-w-minimap track-w))) )
    (add-hook 'window-state-change-hook update-func)
    (demap--add-hook-local 'demap-minimap-kill-hook unhook-func nil buffer) ))

;;track-w construct

(defun demap-track-w-construct()
  ""
  (let ((track-w (demap-track-w-construct-unhooked)))
    (demap-track-w-hook track-w)
    track-w))


(provide 'demap)
;;; demap.el ends here
