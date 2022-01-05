;;; demap--tools.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://github.com/sawyer>
;; Maintainer: Sawyer Gardner <sawyerjgardner@gmail.com>
;; Created: January 04, 2022
;; Modified: January 04, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/sawyer/demap-tool
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;buffer

(defun demap--tools-window-replace-buffer(buffer-or-name new-buffer-or-name)
  "Replace the buffer in all windows holding BUFFER-OR-NAME with NEW-BUFFER-OR-NAME."
  (dolist (window (get-buffer-window-list buffer-or-name t t))
    (set-window-buffer window new-buffer-or-name t) ))

(defun demap--tools-buffer-steal-name(buffer-or-name)
  "Rename BUFFER-OR-NAME and return its old name.
BUFFER-OR-NAME's new name is undefined."
  (with-current-buffer buffer-or-name
    (let ((name (buffer-name)))
      (rename-buffer "-old minimap buffer-" t)
      name )))

(defun demap--tools-real-buffer(buffer)
  "Return base buffer of BUFFER.
if Buffer is not an indirect buffer, return BUFFER.
see `buffer-base-buffer'."
  (or (buffer-base-buffer buffer) buffer))

;;varables

(defun demap--tools-copy-local-variable(variable from-buffer to-buffer)
  "Copy the buffer-local value of VARIABLE in FROM-BUFFER to TO-BUFFER.
if VARABLE is not buffer local in FROM-BUFFER, then
it will no longer be buffer local in TO-BUFFER."
  (if (local-variable-p variable from-buffer)
      (setf (buffer-local-value variable to-buffer) (buffer-local-value variable from-buffer))
    (with-current-buffer to-buffer
      (kill-local-variable variable) )))

(defun demap--tools-list-p(obj)
  "Determin if OBJ is a list.
if OBJ is a list and not a lambda or nil, return t,
otherwise nil."
  (and obj
       (listp obj)
       (not (functionp obj)) ))

;;dolist

(defmacro demap--tools-dolist(spec &rest body)
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
       (if (demap--tools-list-p ,tempvar)
           (dolist (,(nth 0 spec) ,tempvar ,(nth 2 spec))
             ,@body )
         (dolist (,(nth 0 spec) (list ,tempvar) ,(nth 2 spec))
           ,@body )))))

(defmacro demap--tools-dolists-unsafe(specs &rest body)
  "Loop over all SPECS without type checking.
unsafe version of `demap--tools-dolists'.
\(fn ((VAR LIST [STEP])...) BODY...)"
  (declare (indent 1))
  (if specs
      `(demap--tools-dolist ,(car specs)
         (demap--tools-dolists-unsafe ,(cdr specs)
           ,@body ))
    `(progn
       ,@body )))

(defmacro demap--tools-dolists(specs &rest body)
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
  `(demap--tools-dolists-unsafe ,specs
     ,@body))

;;hooks

(defalias 'demap--tools-add-hook    #'add-hook)
(defalias 'demap--tools-remove-hook #'remove-hook)

(defun demap--tools-add-hooks(hooks funcs &optional depth local)
  "Add to the value of HOOKS the functions FUNCS.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

DEPTH and LOCAL are passed to `add-hook'."
  (demap--tools-dolists ((hook hooks)
                   (func funcs) )
    (demap--tools-add-hook hook func depth local)))

(defun demap--tools-remove-hooks(hooks funcs &optional local)
  "Remove FUNCS from the value of HOOKS.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. if function is not in hook, then it is
skipped.

if LOCAL is non-nil then HOOKS are buffer local.
see `remove-hook'."
  (demap--tools-dolists ((hook hooks)
                   (func funcs) )
    (demap--tools-remove-hook hook func local) ))

(defun demap--tools-smart-add-hook(hook func &optional depth local)
  "Add to the value of HOOK the function FUNC and return a cleanup function.
returns a function that, when called, removes FUNC
from HOOK. the returned function excepts no arguments.

DEPTH and LOCAL are passed to `add-hook'."
  (demap--tools-add-hook hook func depth local)
  (if local
      (apply-partially 'demap--tools-remove-hook-local hook func (current-buffer))
    (apply-partially 'demap--tools-remove-hook hook func) ))

(defun demap--tools-smart-add-hooks(hooks funcs &optional depth local)
  "Add to the values of HOOKS the functions FUNCS and return a cleanup function.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

returns a function that, when called, removes FUNCS
from HOOKS. the returned function excepts no arguments.

DEPTH and LOCAL are passed to `add-hook'."
  (demap--tools-add-hooks hooks funcs depth local)
  (if local
      (apply-partially 'demap--tools-remove-hooks-local hooks funcs (current-buffer))
    (apply-partially 'demap--tools-remove-hooks hooks funcs) ))


(defun demap--tools-add-hook-local(hook func &optional depth buffer)
  "Add the function FUNC to the buffer-local value of HOOK as BUFFER.
see `add-hook'."
  (with-current-buffer buffer
    (demap--tools-add-hook hook func depth t) ))

(defun demap--tools-remove-hook-local(hook func &optional buffer)
  "Remove the functions FUNC from the buffer-local values of HOOK as BUFFER.
see `remove-hook'."
  (with-current-buffer buffer
    (demap--tools-remove-hook hook func t) ))

(defun demap--tools-add-hooks-local(hooks funcs &optional depth buffer)
  "Add the functions FUNCS to the buffer-local values of HOOKS as BUFFER.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

DEPTH is passed to `add-hook'."
  (with-current-buffer buffer
    (demap--tools-add-hooks hooks funcs depth t) ))

(defun demap--tools-remove-hooks-local(hooks funcs &optional buffer)
  "Remove the functions FUNCS from the buffer-local values of HOOKS as BUFFER.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. if function is not in hook, then it is
skipped.

see `remove-hook'."
  (with-current-buffer buffer
    (demap--tools-remove-hooks hooks funcs t) ))

(defun demap--tools-smart-add-hook-local(hook func &optional depth buffer)
  "Add FUNC to the buffer-local value of HOOK and return a cleanup function.
returns a function that, when called, removes FUNC
from HOOK. the returned function excepts no arguments.

DEPTH and LOCAL are passed to `add-hook'."
  (with-current-buffer buffer
    (demap--tools-smart-add-hook hook func depth t) ))

(defun demap--tools-smart-add-hooks-local(hooks funcs &optional depth buffer)
  "Add FUNCS to the buffer-local values of HOOKS and return a cleanup function.
HOOKS may be a symbol or a list of symbols, and
FUNCS may be any valid function or a list of valid
functions. functions already in hook are not added.

returns a function that, when called, removes FUNCS
from HOOKS. the returned function excepts no arguments.

DEPTH is passed to `add-hook'."
  (with-current-buffer buffer
    (demap--tools-smart-add-hooks hooks funcs depth t) ))




(provide 'demap--tools)
;;; demap--tools.el ends here
