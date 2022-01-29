;;; demap-track-w.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Sawyer Gardner
;;
;; Author: Sawyer Gardner <https://gitlab.com/sawyerjgardner>
;; Maintainer: Sawyer Gardner <sawyerjgardner@gmail.com>
;; Created: January 04, 2022
;; Modified: January 05, 2022
;; Version: 1.0.0
;; Keywords: convenience extensions lisp
;; Homepage: https://gitlab.com/sawyerjgardner/demap.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Addon to demap-minimap that adds the ability for a minimap to fallow the
;; selected window and an overlay that shows that windows visible region and its
;; current line.
;;
;;
;;
;;
;;; Code:


(require 'cl-lib)
(require 'demap--tools)
(require 'demap-minimap)
(require 'hl-line)


;;;define minimap miner mode-------

(eval-and-compile
  (defun demap--delete-redundant-keys(key seq)
    "Remove KEY and its value from SEQ, skipping the last one."
    (let ((spot     seq)
          (old-spot nil) )
      (while spot
        (when (eq (car spot) key)
          (when old-spot
            (setf (car old-spot) (nth    2 old-spot)
                  (cdr old-spot) (nthcdr 3 old-spot) ))
          (setq old-spot spot) )
        (setq spot (nthcdr 2 spot)) ))
    seq )

  (defun demap--define-mode-var-get-doc(var &optional globalp funcp mode-func mode-pretty-name)
    "Return the documentation that define-miner-mode would give to mode var.
VAR       is the varable symbol.
GLOBALP   is wether the varable is global or local by defalt.
FUNCP     is wether the variable should be set by a function.
MODE-FUNC is the function to set VAR (defalts to VAR).
MODE-PRETTY-NAME is the pretty version of the mode's name."
    (let ((doc-g1 "Non-nil if %s is enabled.
See the `%s' command
for a description of this minor mode." )
          (doc-g2 "
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `%s'." )
          (doc-local "Non-nil if %s is enabled.
Use the command `%s' to change this variable." ))
      (setq mode-func        (or mode-func
                                 var )
            mode-pretty-name (or mode-pretty-name
                                 mode-func ))
      (if globalp
          (concat (format doc-g1 mode-pretty-name mode-func)
                  (when funcp (format doc-g2 mode-func)) )
        (format doc-local mode-pretty-name mode-func) ))))

(defmacro demap--define-mode-var(var init-value &optional globalp funcp doc &rest args)
  "Define a variable VAR the same way define-miner-mode would.
INIT-VALUE defalt value.
GLOBALP    is wether the varable is global or local by defalt.
FUNCP      is wether the variable should be set by a function.
DOC        if not nil, override generated documentation.
ARGS       arguments, see `define-miner-mode'."
  (declare (doc-string 5))
  (if globalp
      (progn
        ;;TODO: maybe remove this line
        (setq args (demap--delete-redundant-keys :require args))
        `(defcustom ,var ,init-value
           ,(or doc (demap--define-mode-var-get-doc var t funcp nil nil))
           ,@(unless (memq :group args)
               '(:group ',(intern (replace-regexp-in-string
                                   "-mode\\'" "" (symbol-name var) ))))
           ,@(unless (memq :set args)
               '(:set #'custom-set-minor-mode) )
           ,@(unless (memq :initialize args)
               '(:initialize 'custom-initialize-default) )
           ,@(unless (memq :type args)
               '(:type 'boolean) )
           ,@args ))
    `(progn
       :autoload-end
       (defvar-local ,var ,init-value
         ,(or doc (demap--define-mode-var-get-doc var nil nil nil nil)) ))))


;;;###autoload
(defmacro demap-define-minimap-miner-mode(mode doc &rest body)
  "Define miner mode for demap minimap buffers.
expanded version of `define-minor-mode'.
modes defined with this macro will only work in a
demap minimap buffer.

this macro also adds a few options:
:protect
        varable or list of variables to copy when
        minimap reconstructs its buffer. the mode
        varable is implicitly protected. notice,
        these variables are made unprotected when
        the mode is disabled, regardless of wether
        other modes are protecting them or not.
:init-func
        form evaluated to set the mode varable to
        true. can also be used to initialize any
        hooks used by this mode. if this form dose
        not set the mode varable to a non-nil
        value, then the mode is still considered
        disabled.
:kill-func
        form evaluated to set the mode varable to
        nil. can also be used to uninitialize any
        hooks used by this mode. this form is also
        evaluated if the mode is active when the
        buffer is killed. if this dose not set the
        mode varable to nil then the mode is
        considered still activated.
:set-func
        a function that sets the value of the mode
        varable. this option overrides :init-func
        and :kill-func. it should be a function
        that accepts one argument (STATE). STATE is
        the state that the mode variable should be
        set to. if the mode variable is not change
        then nether dose the modes state.

the rest of the arguments are passed to
`define-minor-mode'.

\(fn MODE DOC &optional INIT-VALUE LIGHTER KEYMAP &rest BODY)"
  (declare (doc-string 2)
           (indent     1))
  (let (globalp
        init-value
        lighter
        keymap
        (construct-variable t);variable
        (getter    mode)
        (setter    `(setf ,mode))
        after-hook
        (restr     '())

        (protect   '())
        init-func
        kill-func
        set-func )
                                        ;optional args
    (and
     (unless (keywordp (car body))
       (setq init-value (pop body)) )
     (unless (keywordp (car body))
       (setq lighter    (pop body)) )
     (unless (keywordp (car body))
       (setq keymap     (pop body)) ))
                                        ;process keys
    (while (keywordp (car body))
      (let ((key (pop body))
            (val (pop body)) )
        (pcase key
          (:global     (setq globalp    val))
          (:init-value (setq init-value val))
          (:lighter    (setq lighter    (purecopy val)))
          (:keymap     (setq keymap     val))
          (:variable   (let (tmp)
                         (setq construct-variable nil)
                         (if (and (setq tmp (cdr-safe val))
                                  (or (symbolp tmp)
                                      (functionp tmp) ))
                             (setq getter (car val)
                                   setter `(funcall #',tmp) )
                           (setq getter val
                                 setter `(setf ,val) ))))
          (:after-hook (setq after-hook val))
          ;;new
          (:protect    (if (symbolp val)
                           (push val protect)
                         (setq protect (append val protect)) ))
          (:init-func  (setq init-func  val))
          (:kill-func  (setq kill-func  val))
          (:set-func   (setq set-func   val))
          ;;rest
          (_           (setq restr (append (list val key) restr))) )))
                                        ;set defalts
    (setq init-func  (or init-func `(,@setter t  ))
          kill-func  (or kill-func `(,@setter nil))
          set-func   (or set-func
                         (let ((state-var (make-symbol "-state")))
                           `(lambda(,state-var)
                              (if ,state-var
                                  ,init-func
                                ,kill-func )))))
    (when (and construct-variable (not globalp))
      (push mode protect) )
    ;;construct
    `(progn
       ;;variable
       ,@(when construct-variable
           `((demap--define-mode-var ,mode ,init-value
                                     ,globalp ,(and body t) nil
                                     ,@(nreverse restr) )))
       (define-minor-mode ,mode
         ,doc
         ,init-value
         ,lighter
         ,keymap
         :global     ,globalp
         :after-hook (,@(when (and globalp construct-variable)
                          `((when (called-interactively-p 'any)
                              (customize-mark-as-set ',mode) )))
                      ,@after-hook)
         :variable (,getter . (lambda(state)
                                (cl-assert (demap-buffer-minimap) nil "%s can only be used in a demap-minimap buffer" ',mode)
                                (when (xor state ,getter)
                                  ,(if (symbolp set-func)
                                       `(,set-func state)
                                     `(funcall ,set-func state) )
                                  ;;if varable did change
                                  (when (not (xor state ,getter))
                                    (if state
                                        (progn
                                          (demap-minimap-protect-variables t ,@(mapcar (lambda(j) `',j) protect))
                                          (add-hook 'demap-minimap-kill-hook  (apply-partially #',mode 0) nil t) )
                                      (demap-minimap-unprotect-variables t ,@(mapcar (lambda(j) `',j) protect))
                                      (remove-hook 'demap-minimap-kill-hook  (apply-partially #',mode 0) t) )))))
         ,@(nreverse restr)
         ,@body ))))

;;;track-w-mode-------

(defcustom demap-track-w-mode-update-p-func #'demap-track-w-mode-update-p-func-defalt
  "Function to determin if demap-minimap should show the selected window.
the function should accept no arguments. it should
return nil if the current minimap should not show the
selected window."
  :type 'function
  :group 'demap )

;;;###autoload
(demap-define-minimap-miner-mode demap-track-w-mode
  "Minimap miner mode to make minimap show the active window.
makes the minimap this is active in show the buffer
in the currently active window. will not show the
window if `demap-track-w-mode-update-p-func'
returns nil.

this mode can only be used in a demap minimap buffer."
  :group 'demap
  :init-func (progn
               (setf demap-track-w-mode t)
               (add-hook 'window-state-change-hook (apply-partially #'demap-track-w-mode-update-as (demap-buffer-minimap))) )
  :kill-func (progn
               (remove-hook 'window-state-change-hook (apply-partially #'demap-track-w-mode-update-as (demap-buffer-minimap)))
               (kill-local-variable 'demap-track-w-mode) ) )

;;track-w-mode update

(defun demap-track-w-mode-update-p-func-defalt()
  "Determin if track-w mode can fallow the active window.
defalt value for `demap-track-w-mode-update-p-func'
returns nil if the active window's buffer is not a
file buffer."
  (buffer-file-name (window-buffer)) )

(defun demap--track-w-mode-update()
  "Update the window the current minimap is showing.
if the current minimap should not be showing the
active window then tell minimap to sleep."
  (if (funcall demap-track-w-mode-update-p-func)
      (setf (demap-current-minimap-window) (selected-window))
    (demap-current-minimap-window-sleep) ))

(defun demap-track-w-mode-update-as(minimap)
  "Update the window that MINIMAP is showing.
if the MINIMAP should not be showing the active
window then tell it to sleep."
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--track-w-mode-update) ))


;;;current-line-mode-------

(defface demap-current-line-face
  '((t (:inherit hl-line
        :extend  t )))
  "Face used to highlight the current line in demap-minimap."
  :group 'demap )

(defface demap-current-line-inactive-face
  '((t (:inherit demap-current-line-face
        :extend  t )))
  "Face used to highlight the current line in demap-minimap when not active."
  :group 'demap )

;;;###autoload
(demap-define-minimap-miner-mode demap-current-line-mode
  "Minimap miner mode to highlight the current line.
this will use `demap-current-line-face' to
highlight the line, or
`demap-current-line-inactive-face' when the window
the current minimap is showing is not active.

this mode can only be used in a demap minimap buffer."
  :group 'demap
  :init-func (progn
               (setq demap-current-line-mode (make-overlay 0 0))
               (add-hook 'demap-minimap-window-set-hook   #'demap--current-line-mode-activate-if nil t)
               (add-hook 'demap-minimap-window-sleep-hook #'demap--current-line-mode-deactivate  nil t) )

  :kill-func (progn
               (demap--current-line-mode-deactivate)
               (delete-overlay demap-current-line-mode)
               (kill-local-variable 'demap-current-line-mode)
               (remove-hook 'demap-minimap-window-set-hook   #'demap--current-line-mode-activate-if t)
               (remove-hook 'demap-minimap-window-sleep-hook #'demap--current-line-mode-deactivate  t) ))

;;current-line-mode update

(defun demap--current-line-mode-update()
  "Update the position of current-line mode's overlay."
  (let ((w-buffer (window-buffer (demap-current-minimap-window)))
        (m-buffer (current-buffer))
        (showing  (demap-minimap-showing (demap-buffer-minimap)))
        (ov       demap-current-line-mode) )
    (when (eq showing (demap--tools-real-buffer w-buffer))
      (with-current-buffer w-buffer
        (move-overlay ov
                      (line-beginning-position)
                      (+ (line-end-position) 1)
                      m-buffer )))))

(defun demap--current-line-mode-update-has(minimap)
  "Update the position of current-line mode's overlay in MINIMAP."
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--current-line-mode-update) ))

;;current-line-mode activate

(defun demap--current-line-mode-activate()
  "Wake up demap-current-line-mode."
  (overlay-put demap-current-line-mode 'face 'demap-current-line-face)
  (add-hook 'post-command-hook (apply-partially #'demap--current-line-mode-update-has (demap-buffer-minimap)))
  (demap--current-line-mode-update) )

(defun demap--current-line-mode-deactivate()
  "Set demap-current-line-mode to sleep."
  (overlay-put demap-current-line-mode 'face 'demap-current-line-inactive-face)
  (remove-hook 'post-command-hook (apply-partially #'demap--current-line-mode-update-has (demap-buffer-minimap))) )

(defun demap--current-line-mode-activate-if()
  "Set wether demap-current-line-mode should sleep when minimap is not blank."
  (if (demap-current-minimap-window)
      (demap--current-line-mode-activate)
    (demap--current-line-mode-deactivate) ))


;;;visible-region-mode-------

(defface demap-visible-region-face
  '((t (:inherit region
        :extend  t )))
  "Face used to highlight the visible region in demap-minimap."
  :group 'demap )

(defface demap-visible-region-inactive-face
  '((t (:inherit demap-visible-region-face
        :extend  t )))
  "Face used to highlight the visible region in demap-minimap when not active."
  :group 'demap )

;;;###autoload
(demap-define-minimap-miner-mode demap-visible-region-mode
  "minimap miner mode to show the visible region in minimaps window.
this highlights the area in the minimap visible
from the window it is showing. when the window
shown is active, the face
`demap-visible-region-face' is used, otherwise
`demap-visible-region-inactive-face' is used.

this mode can only be used in a demap minimap buffer."
  :group 'demap
  :init-func (progn
               (setq demap-visible-region-mode (make-overlay 0 0))
               (add-hook 'demap-minimap-window-set-hook   #'demap--visible-region-mode-activate-if nil t)
               (add-hook 'demap-minimap-window-sleep-hook #'demap--visible-region-mode-sleep       nil t) )
  :kill-func (progn
               (demap--visible-region-mode-deactivate)
               (delete-overlay demap-visible-region-mode)
               (kill-local-variable 'demap-visible-region-mode)
               (remove-hook 'demap-minimap-window-set-hook   #'demap--visible-region-mode-activate-if t)
               (remove-hook 'demap-minimap-window-sleep-hook #'demap--visible-region-mode-sleep       t) ))

;;visible-region-mode update

(defun demap--visible-region-made-active-p()
  "Determin if demap-visible-region-mode should be active or not.
returns true if the current minimap is showing the active window."
  (let ((window  (demap-current-minimap-window))
        (showing (demap-minimap-showing (demap-buffer-minimap))) )
    (and (window-live-p window)
         (eq (demap--tools-real-buffer (window-buffer window)) showing) )))

(defun demap--visible-region-mode-update()
  "Update the position of demap-visible-region-mode's overlay.
minimap will scroll if overlay goes off screen."
  (let ((window (demap-current-minimap-window)))
    (if (demap--visible-region-made-active-p)
        (let ((ov-start (window-start window))
              (ov-end   (window-end window t)) )
          (move-overlay demap-visible-region-mode
                        ov-start
                        ov-end
                        (current-buffer) )
          (dolist (w (get-buffer-window-list (current-buffer) nil t))
            (when (>= (window-start w) ov-start)
              (set-window-point w ov-start) )
            (when (<= (window-end w t) ov-end)
              (set-window-point w ov-end) )))
      (demap--visible-region-mode-deactivate) )))

(defun demap--visible-region-mode-update-has(minimap &rest i)
  "Update the position of demap-visible-region-mode's overlay in MINIMAP.
I is ignored for function hooks."
  (ignore i)
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--visible-region-mode-update) ))

(defun demap--visible-region-mode-update-window-as(minimap window &rest i)
  "Update the position of demap-visible-region-mode's overlay in MINIMAP.
if MINIMAP is not showing WINDOW then nuthing happens.
I is ignored for function hooks."
  (ignore i)
  (when (eq window (demap-minimap-window minimap) )
    (with-current-buffer (demap-minimap-buffer minimap)
      (demap--visible-region-mode-update) )))

;;visible-region-mode activate

(defun demap--visible-region-mode-activate()
  "Wake up demap-visible-region-mode.
set face and add hooks to update overlay."
  (overlay-put demap-visible-region-mode 'face 'demap-visible-region-face)
  (let ((scrl-func (apply-partially #'demap--visible-region-mode-update-window-as (demap-buffer-minimap)))
        (size-func (apply-partially #'demap--visible-region-mode-update-has       (demap-buffer-minimap))) )
    (add-hook 'window-scroll-functions      scrl-func)
    (add-hook 'window-size-change-functions size-func) )
  (demap--visible-region-mode-update) )

(defun demap--visible-region-mode-deactivate()
  "Put demap-visible-region-mode to sleep.
set face and remove hooks that update overlay."
  (overlay-put demap-visible-region-mode 'face 'demap-visible-region-inactive-face)
  (let ((scrl-func (apply-partially #'demap--visible-region-mode-update-window-as (demap-buffer-minimap)))
        (size-func (apply-partially #'demap--visible-region-mode-update-has       (demap-buffer-minimap))) )
    (remove-hook 'window-scroll-functions      scrl-func)
    (remove-hook 'window-size-change-functions size-func) ))

(defun demap--visible-region-mode-activate-if()
  "Set wether demap-visible-region-mode should sleep when minimap is not blank."
  (if (demap-current-minimap-window)
      (demap--visible-region-mode-activate)
    (demap--visible-region-mode-deactivate) ))

(defun demap--visible-region-mode-sleep()
  "Change demap-visible-region-mode overlay's face to reflect minimap state."
  (overlay-put demap-visible-region-mode 'face 'demap-visible-region-inactive-face) )


(provide 'demap-track-w)
(require 'demap)
;(provide 'demap)
;;; demap-track-w.el ends here
