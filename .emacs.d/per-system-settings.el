(require 'map) ;; Needed for map-merge

(setq bw/system-settings
  (map-merge
    'list
    '((desktop/dpi . 180)
      (desktop/background . "samuel-ferrara-uOi3lg8fGl4-unsplash.jpg")
      (emacs/default-face-size . 220)
      (emacs/variable-face-size . 245)
      (emacs/fixed-face-size . 200)
      (polybar/height . 35)
      (polybar/font-0-size . 18)
      (polybar/font-1-size . 14)
      (polybar/font-2-size . 20)
      (polybar/font-3-size . 13)
      (dunst/font-size . 20)
      (dunst/max-icon-size . 88)
      (vimb/default-zoom . 180)
      (qutebrowser/default-zoom . 200))
    
    (when (equal system-name "crustynubbin")
      '((desktop/dpi . 180)
        (emacs/default-face-size . 190)
        (emacs/variable-face-size . 200)
        (emacs/fixed-face-size . 190)
        (polybar/height . 30)
        (polybar/font-0-size . 16)
        (polybar/font-1-size . 12)
        (polybar/font-2-size . 18)
        (polybar/font-3-size . 11)
        (dunst/font-size . 20)
        (dunst/max-icon-size . 88)
        (vimb/default-zoom . 160)
        (qutebrowser/default-zoom . 180)))
    
    ))
