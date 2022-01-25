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



(defcustom demap-track-w-mode-update-p-func #'demap-track-w-mode-update-p-func-defalt
  "Function to determin if demap-minimap should show the selected window.
the function should accept one argument (TRACK-W)
and return nil if TRACK-W should not show the
selected window. this function is called with
TRACK-W's minimap's buffer has the current buffer."
  :type 'function
  :group 'demap)

(defface demap-current-line-face
  '((((background dark)) (:background "gray50" :extend t))
    (t (:background "gray50" :extend t)))
  "Face used to highlight the current line in demap-minimap."
  :group 'demap)

(defface demap-current-line-inactive-face
  '((((background dark)) (:background "gray77" :extend t))
    (t (:background "gray77" :extend t)))
  "Face used to highlight the current line in demap-minimap when not active."
  :group 'demap)

(defface demap-visible-region-face
  '((((background dark)) (:background "gray10" :extend t))
    (t (:background "gray10" :extend t)))
  "Face used to highlight visible region in demap-minimap."
  :group 'demap)

(defface demap-visible-region-inactive-face
  '((((background dark)) (:background "gray16" :extend t))
    (t (:background "gray16" :extend t)))
  "Face used to highlight visible region in demap-minimap when not active."
  :group 'demap)

;;;define minimap miner mode-------

(eval-and-compile
  (defun demap--delete-redundant-keys(key seq)
    ""
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
    ""
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
  ""
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

(defmacro demap--define-mode-hook(hook mode)
  ""
  `(progn
     (defvar ,hook nil)
     (unless (get ',hook 'variable-documentation)
       (put ',hook 'variable-documentation
            ,(format
              "Hook run after entering or leaving `%s'.
No problems result if this variable is not bound.
`add-hook' automatically binds it.  (This is true for all hook variables.)"
              mode )))))

(defmacro demap--define-mode-keymap-var(var keymap doc)
  ""
  `(defvar ,var
     (let ((km ,keymap))
       (cond ((keymapp km) km)
             ((listp   km) (easy-mmode-define-keymap km))
             (t            (error "Invalid keymap %S" km)) ))
     ,doc ))

(defmacro demap--define-mode-tail(getter lighter keymap-sym keymapp)
  ""
  (let ((modevar (pcase getter
                   (`(default-value ',v) v)
                   (_ getter) )))
    (if (symbolp modevar)
        `(with-no-warnings
           (add-minor-mode ',modevar ',lighter
                           ,(if keymapp
                                keymap-sym
                              `(when (boundp ',keymap-sym)
                                 ,keymap-sym ))))
      (when (or lighter keymapp)
        (error ":lighter and :keymap unsupported with mode expression %S" getter) ))))


(defmacro demap-define-minimap-miner-mode(mode doc &rest body)
  "

\(fn MODE DOC &optional INIT-VALUE LIGHTER KEYMAP &rest BODY)"
  (declare (doc-string 2)
           (indent     1))
  (let (globalp
        init-value
        lighter
        keymap
        (construct-variable t);variable
        (setter mode)
        (getter mode)
        after-hook
        (restr  '())

        (last-message-symbol (make-symbol "last-message"))
        keymap-sym
        hook
        hook-on
        hook-off

        (mode-name (symbol-name mode))
        (protect  '())
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
          (:variable   (setq construct-variable nil)
                       (setq setter     val ;TODO: make this more acuret
                             getter     val ))
          (:after-hook (setq after-hook val))
          ;;new
          (:protect    (if (symbolp val)
                           (push val protect)
                         (setq protect (append val protect)) ))
          (:init-func  (setq init-func  val))
          (:kill-func  (setq kill-func  val))
          (:set-func   (setq set-func   val))
          ;;rest
          (_           (setq restr (append (list val key) restr)) ))))
    ;set defalts
    (setq keymap-sym (if (and keymap (symbolp keymap))
                             keymap
                       (intern (concat mode-name "-map")) )
          hook     (intern (concat mode-name "-hook"    ))
          hook-on  (intern (concat mode-name "-on-hook" ))
          hook-off (intern (concat mode-name "-off-hook")) )
    (setq init-func  (or init-func `(setf ,setter t  ))
          kill-func  (or kill-func `(setf ,setter nil))
          set-func   (or set-func
                         (let ((state-var (make-symbol "--state")))
                           `(lambda(,state-var)
                              (if ,state-var
                                  ,(if (symbolp init-func)
                                       `(,init-func)
                                     init-func )
                                ,(if (symbolp kill-func)
                                     `(,kill-func)
                                   kill-func ))))))
    (when (and construct-variable (not globalp))
      (push mode protect) )
    ;;construct
    `(progn
       ;;variable
       ,@(when construct-variable
           `((demap--define-mode-var ,mode ,init-value
                                    ,globalp ,(and body t) nil
                                    ,@(nreverse restr) )))
       ;;functions
       (defun ,mode (&optional arg)
         ,doc
	 (interactive (list (or current-prefix-arg 'toggle)))
         (let ((,last-message-symbol (current-message)))
           ;;set
           (let ((state (if (eq arg 'toggle)
                            (not ,getter)
                          (> (prefix-numeric-value arg) 0) )))
             (when (xor state ,getter)
               ,(if (symbolp set-func)
                    `(,set-func state)
                  `(funcall ,set-func state) )
                                        ;if varable did change
               (when (not (xor state ,getter))
                 (if state
                     (progn
                       (demap-minimap-protect-variables t ,@(mapcar (lambda(j) `',j) protect))
                       (add-hook 'demap-minimap-kill-hook  (apply-partially ,mode 0) nil t) )
                   (demap-minimap-unprotect-variables t ,@(mapcar (lambda(j) `',j) protect))
                   (remove-hook 'demap-minimap-kill-hook  (apply-partially ,mode 0) t) ))))
           ;;body
           ,@body
           ;;hooks
           (run-hooks ',hook (if ,getter
                                 ',hook-on
                               ',hook-off ))
           (when (called-interactively-p 'any)
             ,(when (and globalp construct-variable)
                `(customize-mark-as-set ',mode) )
             ;; Avoid overwriting a message shown by the body,
             ;; but do overwrite previous messages.
             (unless (and (current-message)
                          (not (equal ,last-message-symbol (current-message))) )
               (message ,(concat "pretty-name"
                                 " %sabled"
                                 (unless globalp " in current buffer") )
                        (if ,getter
                            "en"
                          "dis" ))))
           ;;end
           ,@(when after-hook (list after-hook))
           (force-mode-line-update)
           ,getter ))

       :autoload-end
       (demap--define-mode-hook ,hook ,mode)
       ,@(unless (symbolp keymap)
          `((demap--define-mode-keymap-var ,keymap-sym ,keymap ,(format "Keymap for `%s'." mode))))
       (demap--define-mode-tail ,getter ,lighter ,keymap-sym ,keymap) )))


;;;track-w-mode-------

(demap-define-minimap-miner-mode demap-track-w-mode
  "poo"
  :group 'demap
  :init-func (progn
               (setf demap-track-w-mode t)
               (add-hook 'window-state-change-hook (apply-partially #'demap-track-w-mode-update-as (demap-buffer-minimap))) )
  :kill-func (progn
               (remove-hook 'window-state-change-hook (apply-partially #'demap-track-w-mode-update-as (demap-buffer-minimap)))
               (kill-local-variable 'demap-track-w-mode) ) )

;;track-w-mode update

(defun demap-track-w-mode-update-p-func-defalt()
  "Determin if TRACK-W can fallow the current window."
  (buffer-file-name (window-buffer)) )

(defun demap-track-w-mode-update()
  "Update TRACK-W to test and show current window.
if the current window fails `demap-track-w-window-set'
 then track-w is deactivated."
  (if (funcall demap-track-w-mode-update-p-func)
      (setf (demap-current-minimap-window) (selected-window))
    (demap-current-minimap-window-sleep) ))

(defun demap-track-w-mode-update-as(minimap)
  ""
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap-track-w-mode-update) ))


;;;current-line-mode-------

(demap-define-minimap-miner-mode demap-current-line-mode
  ""
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
  ""
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
  ""
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--current-line-mode-update) ))

;;current-line-mode activate

(defun demap--current-line-mode-activate()
  ""
  (overlay-put demap-current-line-mode 'face 'demap-current-line-face)
  (add-hook 'post-command-hook (apply-partially #'demap--current-line-mode-update-has (demap-buffer-minimap)))
  (demap--current-line-mode-update) )

(defun demap--current-line-mode-deactivate()
  ""
  (overlay-put demap-current-line-mode 'face 'demap-current-line-inactive-face)
  (remove-hook 'post-command-hook (apply-partially #'demap--current-line-mode-update-has (demap-buffer-minimap))) )

(defun demap--current-line-mode-activate-if()
  ""
  (if (demap-current-minimap-window)
      (demap--current-line-mode-activate)
    (demap--current-line-mode-deactivate) ))


;;;test-area-mode-------

(demap-define-minimap-miner-mode demap-test-area-mode
  ""
  :group 'demap
  :init-func (progn
               (setq demap-test-area-mode (make-overlay 0 0))
               (add-hook 'demap-minimap-window-set-hook   #'demap--test-area-mode-activate-if nil t)
               (add-hook 'demap-minimap-window-sleep-hook #'demap--test-area-mode-sleep       nil t) )
  :kill-func (progn
               (demap--test-area-mode-deactivate)
               (delete-overlay demap-test-area-mode)
               (kill-local-variable 'demap-test-area-mode)
               (remove-hook 'demap-minimap-window-set-hook   #'demap--test-area-mode-activate-if t)
               (remove-hook 'demap-minimap-window-sleep-hook #'demap--test-area-mode-sleep       t) ))

;;demap test-area update

(defun demap--test-area-made-active-p()
  "Determin if AREA-OV should be active or not."
  (let ((window  (demap-current-minimap-window))
        (showing (demap-minimap-showing (demap-buffer-minimap))) )
    (and (window-live-p window)
         (eq (demap--tools-real-buffer (window-buffer window)) showing) )))

(defun demap--test-area-mode-update()
  ""
  (let ((window (demap-current-minimap-window)))
    (if (demap--test-area-made-active-p)
        (move-overlay demap-test-area-mode
                      (window-start window)
                      (window-end window t)
                      (current-buffer) )
      (demap--test-area-mode-deactivate) )))

(defun demap--test-area-mode-update-has(minimap &rest i)
  ""
  (ignore i)
  (with-current-buffer (demap-minimap-buffer minimap)
    (demap--test-area-mode-update) ))

(defun demap--test-area-mode-update-window-as(minimap window &rest i)
  ""
  (ignore i)
  (when (eq window (demap-minimap-window minimap) )
    (with-current-buffer (demap-minimap-buffer minimap)
      (demap--test-area-mode-update) )))

;;test-area activate

(defun demap--test-area-mode-activate()
  ""
  (overlay-put demap-test-area-mode 'face 'demap-visible-region-face)
  (let ((scrl-func (apply-partially #'demap--test-area-mode-update-window-as (demap-buffer-minimap)))
        (size-func (apply-partially #'demap--test-area-mode-update-has       (demap-buffer-minimap))) )
    (add-hook 'window-scroll-functions      scrl-func)
    (add-hook 'window-size-change-functions size-func) )
  (demap--test-area-mode-update) )

(defun demap--test-area-mode-deactivate()
  ""
  (overlay-put demap-test-area-mode 'face 'demap-visible-region-inactive-face)
  (let ((scrl-func (apply-partially #'demap--test-area-mode-update-window-as (demap-buffer-minimap)))
        (size-func (apply-partially #'demap--test-area-mode-update-has       (demap-buffer-minimap))) )
    (remove-hook 'window-scroll-functions      scrl-func)
    (remove-hook 'window-size-change-functions size-func) ))

(defun demap--test-area-mode-activate-if()
  ""
  (if (demap-current-minimap-window)
      (demap--test-area-mode-activate)
    (demap--test-area-mode-deactivate) ))

(defun demap--test-area-mode-sleep()
  ""
  (overlay-put demap-test-area-mode 'face 'demap-visible-region-inactive-face) )


(provide 'demap-track-w)
;(provide 'demap)
;;; demap-track-w.el ends here
