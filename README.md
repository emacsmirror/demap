Demap.el
========

Demap is an Emacs package that adds a minimap buffer that shows a zoomed out view
of the active window. It updates what it is showing has you change the active
window and it can be detached, moved, hidden or killed like any other Emacs
buffer.

Screenshots
-----------

![Demap in defalt theme](https://drive.google.com/uc?export=view&id=1BewWlI9-GsihrRZzMpgY54iLu9SrWUVc) | ![Demap in darker blue theme](https://drive.google.com/uc?export=view&id=15wQ8ReWQM7h0ROxHaEV8C277IPwN1axw)
-|-
![Demap in doom-one theme](https://drive.google.com/uc?export=view&id=1i2z4dBbZnyLZqLzJEvaXIeIXmNz-FeIC) |

That last one is setup with [Solaire-mode](https://github.com/hlissner/emacs-solaire-mode), [Doom Emacs](https://github.com/hlissner/doom-emacs), the theme [doom-one](https://github.com/doomemacs/themes), and the font [Minimap](https://github.com/davestewart/minimap-font).
The settings in my config.el used are:

``` emacs-lisp
(setq doom-theme 'doom-one)
(after! (solaire-mode demap)
  (demap-set-open-options 'right 15)
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

You can do <kdb>M-x demap-minimap-toggle</kdb> to open and close the defalt minimap to the
side of the active frame. If you just want to make a standalone minimap buffer, do
<kdb>M-x demap-minimap-construct</kdb>.

Configuration
-------------

The side and width of the window that <kdb>demap-minimap-toggle</kdb> opens to show the
minimap can be set with the function <kdb>demap-set-open-options</kdb>. Using it can also
fix problems with <kdb>display-buffer-alist</kdb> capturing minimap buffers.

Removing <kdb>demap-current-line-mode</kdb> or <kdb>demap-visible-region-mode</kdb> from the
hook <kdb>demap-minimap-construct-hook</kdb> will disable the corresponding
overlays.

``` emacs-lisp
(remove-hook 'demap-minimap-construct-hook 'demap-current-line-mode)
(remove-hook 'demap-minimap-construct-hook 'demap-visible-region-mode)
```

You can change what windows the minimap will show by setting the function in
<kdb>demap-track-window-mode-update-p-func</kdb>. For example, you can make minimaps only
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
