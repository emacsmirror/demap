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


;;minimap buffer

(defun demap-buffer-minimap(&optional buffer-or-name)
  "Return the demap-minimap associated with BUFFER-OR-NAME.
If BUFFER-OR-NAME is not associated with a minimap then it returns nil."
  (buffer-local-value 'demap--current-minimap (window-normalize-buffer buffer-or-name)))

(defun demap--buffer-run-change-functions(buffer minimap)
  "Run hook 'demap-minimap-change-functions' has BUFFER, with MINIMAP."
  (with-current-buffer buffer
    (run-hook-with-args 'demap-minimap-change-functions minimap) ))

(defun demap--kill-old-minimap-buffer(minimap-buffer minimap)
  "Kill buffer MINIMAP-BUFFER that used to be associated with MINIMAP."
  (unwind-protect
      (demap--buffer-run-change-functions minimap-buffer minimap)
    (kill-buffer minimap-buffer) ))


(defun demap--generate-name(&optional name)
  "Return a name that can be used for a minimap buffer.
NAME overrides the defalt name defined by 'demap-defalt-buffer-name'."
  (generate-new-buffer-name (or name demap-defalt-buffer-name)))

(defun demap--make-indirect-buffer(name &optional buffer-show)
  "Make a new indirect buffer with name NAME and showing BUFFER-SHOW.
if BUFFER-SHOW is nul then it returns a blank buffer."
  (if buffer-show
      (make-indirect-buffer buffer-show name)
    (generate-new-buffer name) ))

(defun demap--generate-buffer(&optional name buffer-show)
  "Make a new buffer for demap-minimap using name NAME and showing BUFFER-SHOW."
  (demap--make-indirect-buffer (demap--generate-name name) buffer-show) )

(defun demap--generate-minimap-buffer(&optional name buffer-show)
  "Make and setup a buffer for demap-minimap with name NAME and showing BUFFER-SHOW."
  (with-current-buffer (demap--generate-buffer name buffer-show)
    (make-local-variable 'auto-hscroll-mode)
    (buffer-face-set 'demap-font-face)
    (setq vertical-scroll-bar nil
          truncate-lines      t
          buffer-read-only    t
          auto-hscroll-mode   nil)
    (current-buffer) ))

(defun demap--remake-minimap-buffer(old-buffer-or-name buffer-show)
  "Make a copy of OLD-BUFFER-OR-NAME but have it show BUFFER-SHOW."
  (demap--generate-minimap-buffer (demap--buffer-steal-name old-buffer-or-name) buffer-show) )


;;minimap object

(defun demap-minimap-buffer(minimap)
  "Return the buffer used by MINIMAP.
this buffer can get killed when the minimap
switches what buffer it is shadowing."
  (nth 1 minimap))

(defun demap-minimap-p(minimap)
  "Determin if MINIMAP is a demap-minimap."
  (and (listp minimap)
       (eq (car minimap) 'demap-minimap) ))

(defun demap-minimap-live-p(minimap)
  "Determin if MINIMAP is live."
  (and (demap-minimap-p minimap)
       (buffer-live-p (demap-minimap-buffer minimap)) ))

(defun demap-normalize-minimap(minimap-or-name)
  "Return demap-minimap specified by MINIMAP-OR-NAME.
MINIMAP-OR-NAME must be a live minimap, a live
minimap-buffer, a string naming a live
minimap-buffer or nil which means to return the
minimap for the current buffer."
  (if (demap-minimap-p minimap-or-name)
      minimap-or-name
    (or (demap-buffer-minimap minimap-or-name)
        (error "No such demap-minimap: %s" minimap-or-name) )))

(defun demap-minimap-showing(&optional minimap-or-name)
  "Return the buffer that MINIMAP-OR-NAME is showing.
if MINIMAP-OR-NAME is blank or dead, return nil."
  (let ((minimap (demap-normalize-minimap minimap-or-name)))
    (when (demap-minimap-live-p minimap)
      (buffer-base-buffer (demap-minimap-buffer minimap)) )))


(defun demap--minimap-buffer-set(minimap buffer-or-name)
  "Set the buffer used by minimap MINIMAP to BUFFER-OR-NAME.
identical to (setf ('demap-minimap-buffer' MINIMAP) BUFFER-OR-NAME)"
  (let ((bfr (window-normalize-buffer buffer-or-name)))
    (when (demap-minimap-live-p minimap)
      (with-current-buffer (demap-minimap-buffer minimap)
        (kill-local-variable 'demap--current-minimap) ))
    (with-current-buffer bfr
      (setq demap--current-minimap minimap) )
    (setf (nth 1 minimap) bfr) ))

(defun demap--minimap-swapout-buffer(minimap minimap-buffer)
  "Replace the buffer in minimap MINIMAP with MINIMAP-BUFFER."
  (demap--window-replace-buffer (demap-minimap-buffer minimap) minimap-buffer)
  (demap--minimap-buffer-set minimap minimap-buffer) )

(defun demap--unsafe-minimap-showing-set(minimap new-show)
  "Version of ('demap-minimap-showing-set' MINIMAP NEW-SHOW) without type check."
  (let ((old-minimap-buffer (demap-minimap-buffer minimap)))
    (demap--minimap-swapout-buffer minimap (demap--remake-minimap-buffer old-minimap-buffer new-show))
    (demap--kill-old-minimap-buffer old-minimap-buffer minimap) ))

(defun demap-minimap-showing-set(minimap-or-name buffer-or-name)
  "Set the buffer that minimap MINIMAP-OR-NAME is showing to BUFFER-OR-NAME.
this is equivalent to (setf ('demap-minimap-showing' MINIMAP-OR-NAME) BUFFER-OR-NAME)"
  (demap--unsafe-minimap-showing-set
   (demap-normalize-minimap minimap-or-name)
   (window-normalize-buffer buffer-or-name))
  buffer-or-name )


(gv-define-setter demap-minimap-buffer(buffer-or-name minimap)
  `(demap--minimap-buffer-set ,minimap ,buffer-or-name))

(gv-define-setter demap-minimap-showing(buffer-or-name minimap-or-name)
  `(demap-minimap-showing-set ,minimap-or-name ,buffer-or-name))


(defun demap-generate-minimap(&optional name)
  "Genorate and return a new minimap with name NAME."
  (let ((new-map `(demap-minimap nil)))
    (setf (demap-minimap-buffer new-map) (demap--generate-minimap-buffer name))
    new-map ))


;;minimap region

(defun demap-region-p(region)
  "Determin if REGION is a demap-region object."
  (and (listp region)
       (eq (car region) 'demap-region) ))


(defun demap-region-overlay(region)
  "Get the overlay object used by demap-region REGION."
  (nth 1 region) )

(defun demap-region-change-f(region)
  "Get the function called when REGION's minimap change."
  (nth 2 region) )

(defun demap-region-update-f(region)
  "Get the function called when REGION needs to update its placement."
  (nth 3 region))

(defun demap-region-buffer(region)
  "Get the buffer that REGION is in."
  (overlay-buffer (demap-region-overlay region)) )

(defun demap-region-minimap(region)
  "Get the demap-minimap that REGION is in."
  (demap-buffer-minimap (demap-region-buffer region)) )


(defun demap-region-change-f-attach(region)
  ""
  (with-current-buffer (demap-region-buffer region)
    (add-hook 'demap-minimap-change-functions (demap-region-change-f region) 0 t) ))

(defun demap-region-change-f-dettach(region)
  ""
  (with-current-buffer (demap-region-buffer region)
    (remove-hook 'demap-minimap-change-functions (demap-region-change-f region) t) ))

(defun demap--region-update-f-call-move(region buffer)
  ""
  (funcall (demap-region-update-f region) region buffer))

(defun demap-region-update-f-call(region)
  ""
  (demap--region-update-f-call-move region (demap-region-buffer region)) )

(defun demap-region-update-f-defalt(region buffer)
  ""
  (move-overlay (demap-region-overlay region) 0 50 buffer) )


(defun demap--region-overlay-set(region overlay)
  "Set the overlay used by REGION to OVERLAY."
  (setf (nth 1 region) overlay) )

(defun demap--region-change-f-set(region function)
  "Set the function called when REGION's minimap change to FUNCTION."
  (when (demap-region-change-f region)
    (demap-region-change-f-dettach region) )
  (setf (nth 2 region) function)
  (demap-region-change-f-attach region)
  function )

(defun demap-region-update-f-set(region overlay)
  "Set the overlay used by REGION to OVERLAY."
  (setf (nth 3 region) overlay) )

(defun demap--region-buffer-set(region buffer)
  "Set the buffer that REGION is in to BUFFER."
  (demap-region-change-f-dettach region)
  (demap--region-update-f-call-move region buffer)
  (demap-region-change-f-attach region)
  buffer )

(defun demap-region-minimap-set(region minimap)
  "Set the demap-minimap that REGION is in to MINIMAP."
  (demap--region-buffer-set region (demap-minimap-buffer minimap))
  minimap )


(gv-define-setter demap-region-overlay(overlay region)
  `(demap--region-overlay-set ,region ,overlay))

(gv-define-setter demap-region-change-f(funct region)
  `(demap--region-change-f-set ,region ,funct))

(gv-define-setter demap-region-update-f(funct region)
  `(demap-region-update-f-set ,region ,funct))

(gv-define-setter demap-region-buffer(buffer region)
  `(demap--region-buffer-set ,region ,buffer))

(gv-define-setter demap-region-minimap(minimap region)
  `(demap-region-minimap-set ,region ,minimap))


(defun demap--generate-overlay(buffer)
  "Generate overlay for demap-region in buffer BUFFER."
  (let ((overlay (make-overlay 0 50 buffer)))
    (overlay-put overlay 'face 'demap-current-line-face)
    overlay ))

(defun demap--generate-region-overlay(minimap)
  "Generate region for demap-region in minimap MINIMAP."
  (demap--generate-overlay (demap-minimap-buffer minimap)) )

(defun demap--genorate-region-change-f(region)
  ""
  (apply-partially #'demap-region-minimap-set region))

(defun demap--genorate-region-update-f()
  ""
  #'demap-region-update-f-defalt )


(defun demap-generate-region(minimap)
  "Make a new overlay object for MINIMAP using CHANGE-FUNCTION."
  (let ((region '(demap-region nil nil nil)))
    (setf (demap-region-overlay region) (demap--generate-region-overlay minimap)
          (demap-region-change-f region) (demap--genorate-region-change-f region)
          (demap-region-update-f region) (demap--genorate-region-update-f) )
    region ))


;;page and line

(defun demap-page-region-update-f(region buffer)
  ""
  (move-overlay (demap-region-overlay region)
                (window-start)
                (window-end nil t)
                buffer ))

(defun demap-line-region-update-f(region buffer)
  ""
  (move-overlay (demap-region-overlay region)
                (line-beginning-position)
                (line-end-position)
                buffer ))

(defun demap-generate-test-region(minimap)
  ""
  (let ((region (demap-generate-region minimap)))
    (setf (demap-region-update-f region) #'demap-test-update-f)
    region ))


;;fallow mode

(defvar-local demap-fallow-mode nil)


(defun demap--fallow-init()
  "Initalize demap-fallow-mode."
  (message "initing")
  (setq demap-fallow-mode t) )

(defun demap--fallow-close()
  "Initalize demap-fallow-mode."
  (message "closeing")
  (setq demap-fallow-mode nil) )


(defun demap--should-toggle-mode-p(mode arg)
  "Determin if mode MODE should toggle if passed ARG."
  (or (eq arg 'toggle)
      (xor (> (prefix-numeric-value arg) 0) mode) ))

(defun demap-fallow-mode(&optional arg)
  "Toggle demap-minimap-minermode ARG."
  (interactive (list (or current-prefix-arg 'toggle)))
  ;(cl-assert (demap-buffer-minimap) nil "demap-fallow-mode only works on a demap-minimap buffer.")
  (when (demap--should-toggle-mode-p demap-fallow-mode arg)
    (if demap-fallow-mode
        (demap--fallow-close)
      (demap--fallow-init) )))



(provide 'demap)
;;; demap.el ends here
