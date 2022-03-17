Demap.el
========

Demap is an Emacs package that adds a minimap buffer that shows a zoomed out view
of the active window. It updates what it is showing has you change the active
window and it can be detached, moved, hidden or killed like any other Emacs
buffer.

Screenshots
-----------

![Demap in default theme](https://drive.google.com/uc?export=view&id=1BewWlI9-GsihrRZzMpgY54iLu9SrWUVc) | ![Demap in darker blue theme](https://drive.google.com/uc?export=view&id=15wQ8ReWQM7h0ROxHaEV8C277IPwN1axw)
-|-
![Demap in leuven theme](https://drive.google.com/uc?export=view&id=1VLprbi2G9TJiBcu19ma-EpyeLjNxsz7L) | ![Demap in doom-one theme](https://drive.google.com/uc?export=view&id=1i2z4dBbZnyLZqLzJEvaXIeIXmNz-FeIC) 

That last screenshot has [Solaire-mode](https://github.com/hlissner/emacs-solaire-mode), [Doom Emacs](https://github.com/hlissner/doom-emacs), the theme [doom-one](https://github.com/doomemacs/themes), and the font [Minimap](https://github.com/davestewart/minimap-font). The configuration used for it is:
``` emacs-lisp
(setq doom-theme 'doom-one)
(after! (solaire-mode demap)
  (setq demap-minimap-window-width 15)
  (let ((gray1 "#1A1C22")
        (gray2 "#21242b")
        (gray3 "#282c34")
        (gray4 "#2b3038") )
    (face-spec-set 'demap-minimap-font-face
                   `((t :background ,gray2
                        :inherit    unspecified
                        :family     "minimap"
                        :height     10          )))
    (face-spec-set 'demap-visible-region-face
                   `((t :background ,gray4
                        :inherit    unspecified )))
    (face-spec-set 'demap-visible-region-inactive-face
                   `((t :background ,gray3
                        :inherit    unspecified )))
    (face-spec-set 'demap-current-line-face
                   `((t :background ,gray1
                        :inherit    unspecified )))
    (face-spec-set 'demap-current-line-inactive-face
                   `((t :background ,gray1
                        :inherit    unspecified )))))

```

Usage
-----

You can do `M-x demap-toggle` to open and close the default minimap to the
side of the active frame. If you just want to make a standalone minimap buffer, do
`M-x demap-minimap-construct`.

Configuration
-------------

The side and width of the window that `demap-toggle` opens to show the
minimap can be changed by setting the variables `demap-minimap-window-side` or
`demap-minimap-window-width`. 

``` emacs-lisp
(setq demap-minimap-window-side  'left)
(setq demap-minimap-window-width 15)
```

You can change what windows a minimap can show by setting the function in
`demap-track-window-mode-update-p-func`. For example, you can make minimaps only
show windows in the same frame has it by using:

``` emacs-lisp
(defun my-track-window-update-p()
  "my minimap update predicate function."
  (and (demap-track-w-mode-update-p-func-default)
       (get-buffer-window) ))

(setq demap-track-window-mode-update-p-func #'my-track-window-update-p)
```

Indicate When Active
--------------------

when the window the minimap is showing is not the active window, the current
line and visible region overlays change face's from `demap-current-line-face`
and `demap-visible-region-face` to `demap-current-line-inactive-face` and
`demap-visible-region-inactive-face` respectively. By default these new faces are
the same as the usual ones, but these faces can be changed to have a visual
indication of whether you are in the window the minimap is showing or not.
