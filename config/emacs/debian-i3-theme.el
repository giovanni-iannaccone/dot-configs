(deftheme debian-i3
  "Debian i3 theme")

(custom-theme-set-faces
 'debian-i3

 '(default
   ((t (:background "#161616"
                    :foreground "#D8DEE9"
                    :family "JetBrainsMono Nerd Font"))))

 '(cursor
   ((t (:background "#00A8FF"))))

 '(region
   ((t (:background "#0057FF"
                    :foreground "#FFFFFF"))))

 '(highlight
   ((t (:background "#222222"))))

 '(minibuffer-prompt
   ((t (:foreground "#008CFF"))))

 '(mode-line
   ((t (:background "#222222"
                    :foreground "#D8DEE9"
                    :box nil))))

 '(mode-line-inactive
   ((t (:background "#161616"
                    :foreground "#666666"
                    :box nil))))
 ;; Syntax

 ;; comments
 '(font-lock-comment-face
   ((t (:foreground "#607080"
                    :slant italic))))

 ;; strings
 '(font-lock-string-face
   ((t (:foreground "#8FB339"))))

 ;; functions
 '(font-lock-function-name-face
   ((t (:foreground "#4C9ED9"))))

 ;; keywords
 '(font-lock-keyword-face
   ((t (:foreground "#008CFF"))))

 ;; variables
 '(font-lock-variable-name-face
   ((t (:foreground "#D8DEE9"))))

 ;; types / libraries
 '(font-lock-type-face
   ((t (:foreground "#39B54A"))))

 ;; constants
 '(font-lock-constant-face
   ((t (:foreground "#00B7FF"))))

 ;; numbers
 '(font-lock-number-face
   ((t (:foreground "#73D216"))))

 ;; warnings/errors
 '(font-lock-warning-face
   ((t (:foreground "#FF0033"
                    :weight bold))))

 ;; ============================================================
 ;; Search
 ;; ============================================================

 '(isearch
   ((t (:background "#0057FF"
                    :foreground "#FFFFFF"
                    :weight bold))))

 '(lazy-highlight
   ((t (:background "#30363D"
                    :foreground "#D8DEE9"))))


 ;; ============================================================
 ;; Links
 ;; ============================================================

 '(link
   ((t (:foreground "#00E5FF"
                    :underline t))))


 ;; ============================================================
 ;; Vterm
 ;; ============================================================

 '(vterm-color-default
   ((t (:foreground "#D8DEE9"
                    :background "#161616"))))

 ;; Normal ANSI
 '(vterm-color-black
   ((t (:foreground "#000000"))))

 '(vterm-color-red
   ((t (:foreground "#FF0033"))))

 '(vterm-color-green
   ((t (:foreground "#00FF41"))))

 '(vterm-color-yellow
   ((t (:foreground "#FFD600"))))

 '(vterm-color-blue
   ((t (:foreground "#008CFF"))))

 '(vterm-color-magenta
   ((t (:foreground "#D500FF"))))

 '(vterm-color-cyan
   ((t (:foreground "#00E5FF"))))

 '(vterm-color-white
   ((t (:foreground "#E8F1FF"))))

 ;; Bright ANSI
 '(vterm-color-bright-black
   ((t (:foreground "#333333"))))

 '(vterm-color-bright-red
   ((t (:foreground "#FF3355"))))

 '(vterm-color-bright-green
   ((t (:foreground "#39FF14"))))

 '(vterm-color-bright-yellow
   ((t (:foreground "#FFFF33"))))

 '(vterm-color-bright-blue
   ((t (:foreground "#00B7FF"))))

 '(vterm-color-bright-magenta
   ((t (:foreground "#FF33FF"))))

 '(vterm-color-bright-cyan
   ((t (:foreground "#66FFFF"))))

 '(vterm-color-bright-white
   ((t (:foreground "#FFFFFF"))))

)

(provide-theme 'debian-i3)
