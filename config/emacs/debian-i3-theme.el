(deftheme debian-i3
  "Debian i3 theme")

(custom-theme-set-faces
 'debian-i3

 ;; Base
 '(default ((t (:background "#161616"
                            :foreground "#D8DEE9"
                            :family "JetBrainsMono Nerd Font"))))

 '(cursor ((t (:background "#4C7899"))))
 '(region ((t (:background "#1F405A"))))
 '(highlight ((t (:background "#222222"))))
 '(minibuffer-prompt ((t (:foreground "#4C7899"))))


 ;; Mode line
 '(mode-line ((t (:background "#222222"
                              :foreground "#D8DEE9"
                              :box nil))))
 '(mode-line-inactive ((t (:background "#161616"
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

 ;; keyword
 '(font-lock-keyword-face
   ((t (:foreground "#008CFF"))))

 ;; variables
 '(font-lock-variable-name-face
   ((t (:foreground "#D8DEE9"))))

 ;; types
 '(font-lock-type-face
   ((t (:foreground "#39B54A"))))

 ;; constants
 '(font-lock-constant-face
   ((t (:foreground "#00B7FF"))))

 ;; numbers
 '(font-lock-number-face
   ((t (:foreground "#73D216"))))

 ;; warning/errors
 '(font-lock-warning-face
   ((t (:foreground "#FF0033"
                    :weight bold))))


 ;; Search
 '(isearch
   ((t (:background "#4C7899"
                    :foreground "#000000"
                    :weight bold))))

 '(lazy-highlight
   ((t (:background "#30363D"
                    :foreground "#D8DEE9"))))


 ;; Link
 '(link
   ((t (:foreground "#00B7FF"
                    :underline t))))

)

(provide-theme 'debian-i3)
