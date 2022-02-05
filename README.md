demap.el
========

this is an emacs package that adds a minimap buffer that shows a zoomed out view
of the active window. it updates what it is showing has you change the active
window and it can be detached, moved, hidden or killed like any other emacs
buffer.

Screenshots
-----------

Usage
-----

you can do <kdb>M-x demap-minimap-toggle</kdb> to open and close the defalt minimap to the
side of the active frame. if you just want to make a standalone minimap buffer, do
<kdb>M-x demap-minimap-construct</kdb>.

Configuration
-------------

the side and width of the window that <kdb>demap-minimap-toggle</kdb> opens to show the
minimap can be set with the function <kdb>demap-set-open-options</kdb>. using it can also
fix problems with <kdb>display-buffer-alist</kdb> capturing minimap buffers.

removing <kdb>demap-current-line-mode</kdb> or <kdb>demap-visible-region-mode</kdb> from the
hook <kdb>demap-minimap-construct-hook</kdb> will disable the corresponding
overlays.

``` emacs-lisp
(remove-hook 'demap-minimap-construct-hook 'demap-current-line-mode)
(remove-hook 'demap-minimap-construct-hook 'demap-visible-region-mode)
```

you can change what windows the minimap will show by setting the function in
<kdb>demap-track-window-mode-update-p-func</kdb>. for example, you can make minimaps only
show windows in the same frame has it by using:

``` emacs-lisp
(defun my-track-window-update-p()
  "my minimap update predicate function."
  (and (demap-track-w-mode-update-p-func-defalt)
       (get-buffer-window) ))

(setq demap-track-window-mode-update-p-func #'my-track-window-update-p)
```

Indicate When Active
--------------------
