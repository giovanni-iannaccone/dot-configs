(deftheme debian-i3
  "Debian i3 theme")

(custom-theme-set-faces
 'debian-i3

 '(default
   ((t (:background "#000000"
                    :foreground "#D8DEE9"
                    :family "JetBrainsMono Nerd Font"))))

 '(fringe
   ((t (:background "#000000"
                    :foreground "#607080"))))

 '(cursor
   ((t (:background "#00A8FF"))))

 '(region
   ((t (:background "#0057FF"
                    :foreground "#FFFFFF"))))

 '(highlight
   ((t (:background "#151515"
                    :foreground "#D8DEE9"))))

 '(minibuffer-prompt
   ((t (:foreground "#008CFF"
                    :weight bold))))

 '(mode-line
   ((t (:background "#0A0A0A"
                    :foreground "#D8DEE9"
                    :box nil))))

 '(mode-line-inactive
   ((t (:background "#000000"
                    :foreground "#444444"
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

 '(isearch
   ((t (:background "#0057FF"
                    :foreground "#FFFFFF"
                    :weight bold))))

 '(lazy-highlight
   ((t (:background "#30363D"
                    :foreground "#D8DEE9"))))

 '(link
   ((t (:foreground "#00E5FF"
                    :underline t))))

 ;; Eat / ANSI colors
 '(ansi-color-black
   ((t (:foreground "#000000"))))

 '(ansi-color-red
   ((t (:foreground "#FF0033"))))

 '(ansi-color-green
   ((t (:foreground "#00FF41"))))

 '(ansi-color-yellow
   ((t (:foreground "#FFD600"))))

 '(ansi-color-blue
   ((t (:foreground "#008CFF"))))

 '(ansi-color-magenta
   ((t (:foreground "#D500FF"))))

 '(ansi-color-cyan
   ((t (:foreground "#00E5FF"))))

 '(ansi-color-white
   ((t (:foreground "#E8F1FF"))))

 '(ansi-color-bright-black
   ((t (:foreground "#333333"))))

 '(ansi-color-bright-red
   ((t (:foreground "#FF3355"))))

 '(ansi-color-bright-green
   ((t (:foreground "#39FF14"))))

 '(ansi-color-bright-yellow
   ((t (:foreground "#FFFF33"))))

 '(ansi-color-bright-blue
   ((t (:foreground "#00B7FF"))))

 '(ansi-color-bright-magenta
   ((t (:foreground "#FF33FF"))))

 '(ansi-color-bright-cyan
   ((t (:foreground "#66FFFF"))))

 '(ansi-color-bright-white
   ((t (:foreground "#FFFFFF"))))
 )

(provide-theme 'debian-i3)
