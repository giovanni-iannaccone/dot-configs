;;; debian-i3.el --- Debian i3 theme -*- lexical-binding: t; -*-

(deftheme debian-i3
  "Debian i3 theme")

(defgroup debian-i3 nil
  "Customization for the Debian i3 theme."
  :group 'faces)

(let* (
       (bg             "#000000")
       (bg-alt         "#0A0A0A")
       (bg-highlight   "#151515")
       (bg-selection   "#0057FF")

       (fg             "#D8DEE9")
       (fg-bright      "#FFFFFF")
       (fg-muted       "#607080")
       (fg-dim         "#444444")
       (fg-guide       "#30363D")

       (blue           "#00B7FF")
       (blue-bright    "#00A8FF")
       (cyan           "#00E5FF")
       (green          "#00CC00")

       (yellow         "#FFD600")
       (red            "#FF0033")
       (red-bright     "#FF3355")
       (magenta        "#D500FF")

       (green-bg       "#001400")
       (green-bg-hl    "#002000")
       (red-bg         "#140006")
       (red-bg-hl      "#200009")

       (ansi-white     "#E8F1FF")
       (ansi-bright-cyan "#66FFFF"))

  (custom-theme-set-faces
   'debian-i3

   `(default
     ((t (:background ,bg
                      :foreground ,fg
                      :family "JetBrainsMono Nerd Font"))))

   `(fringe
     ((t (:background ,bg
                      :foreground ,fg-dim))))

   `(cursor
     ((t (:background ,blue))))

   `(region
     ((t (:background ,bg-selection
                      :foreground ,fg-bright))))

   `(highlight
     ((t (:background ,bg-highlight
                      :foreground ,fg))))

   `(minibuffer-prompt
     ((t (:foreground ,blue
                      :weight bold))))

   `(mode-line
     ((t (:background ,bg-alt
                      :foreground ,fg
                      :box nil))))

   `(mode-line-inactive
     ((t (:background ,bg
                      :foreground ,fg-dim
                      :box nil))))

   `(header-line
     ((t (:background ,bg
                      :foreground ,fg))))

   `(vertical-border
     ((t (:foreground ,fg-guide))))

   `(shadow
     ((t (:foreground ,fg-muted))))

   `(font-lock-comment-face
     ((t (:foreground ,fg-muted
                      :slant italic))))

   `(font-lock-string-face
     ((t (:foreground ,green))))

   `(font-lock-function-name-face
     ((t (:foreground ,cyan))))

   `(font-lock-keyword-face
     ((t (:foreground ,blue
                      :weight bold))))

   `(font-lock-variable-name-face
     ((t (:foreground ,fg))))

   `(font-lock-type-face
     ((t (:foreground ,green))))

   `(font-lock-constant-face
     ((t (:foreground ,cyan))))

   `(font-lock-number-face
     ((t (:foreground ,green))))

   `(font-lock-warning-face
     ((t (:foreground ,red
                      :weight bold))))

   `(font-lock-doc-face
     ((t (:foreground ,fg-muted))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,blue
                      :weight bold))))

   `(font-lock-builtin-face
     ((t (:foreground ,cyan))))

   `(font-lock-negation-char-face
     ((t (:foreground ,red
                      :weight bold))))

   `(font-lock-regexp-grouping-backslash
     ((t (:foreground ,blue))))

   `(font-lock-regexp-grouping-construct
     ((t (:foreground ,cyan))))

   `(isearch
     ((t (:background ,bg-selection
                      :foreground ,fg-bright
                      :weight bold))))

   `(isearch-fail
     ((t (:background ,red
                      :foreground ,fg-bright
                      :weight bold))))

   `(lazy-highlight
     ((t (:background ,bg-highlight
                      :foreground ,fg))))

   `(match
     ((t (:background ,bg-selection
                      :foreground ,fg-bright
                      :weight bold))))

   `(query-replace
     ((t (:background ,bg-selection
                      :foreground ,fg-bright
                      :weight bold))))

   `(link
     ((t (:foreground ,cyan
                      :underline t))))

   `(link-visited
     ((t (:foreground ,magenta
                      :underline t))))

   `(ansi-color-black
     ((t (:foreground ,bg))))

   `(ansi-color-red
     ((t (:foreground ,red))))

   `(ansi-color-green
     ((t (:foreground ,green))))

   `(ansi-color-yellow
     ((t (:foreground ,yellow))))

   `(ansi-color-blue
     ((t (:foreground ,blue))))

   `(ansi-color-magenta
     ((t (:foreground ,magenta))))

   `(ansi-color-cyan
     ((t (:foreground ,cyan))))

   `(ansi-color-white
     ((t (:foreground ,ansi-white))))

   `(ansi-color-bright-black
     ((t (:foreground ,fg-dim))))

   `(ansi-color-bright-red
     ((t (:foreground ,red-bright))))

   `(ansi-color-bright-green
     ((t (:foreground ,green))))

   `(ansi-color-bright-yellow
     ((t (:foreground ,yellow))))

   `(ansi-color-bright-blue
     ((t (:foreground ,blue-bright))))

   `(ansi-color-bright-magenta
     ((t (:foreground ,magenta))))

   `(ansi-color-bright-cyan
     ((t (:foreground ,ansi-bright-cyan))))

   `(ansi-color-bright-white
     ((t (:foreground ,fg-bright))))

   `(magit-section-heading
     ((t (:foreground ,blue
                      :weight bold))))

   `(magit-section-secondary-heading
     ((t (:foreground ,fg-muted
                      :weight bold))))

   `(magit-section-highlight
     ((t (:background ,bg-highlight
                      :foreground ,fg))))

   `(magit-section-heading-selection
     ((t (:foreground ,cyan
                      :weight bold))))

   `(magit-branch-local
     ((t (:foreground ,blue))))

   `(magit-branch-remote
     ((t (:foreground ,green))))

   `(magit-branch-current
     ((t (:foreground ,cyan
                      :weight bold))))

   `(magit-branch-upstream
     ((t (:foreground ,fg-muted))))

   `(magit-branch-warning
     ((t (:foreground ,yellow
                      :weight bold))))

   `(magit-log-author
     ((t (:foreground ,fg-muted))))

   `(magit-log-date
     ((t (:foreground ,fg-dim))))

   `(magit-log-graph
     ((t (:foreground ,fg-guide))))

   `(magit-log-head
     ((t (:foreground ,blue
                      :weight bold))))

   `(magit-log-reflog
     ((t (:foreground ,fg-muted))))

   `(magit-hash
     ((t (:foreground ,fg-muted))))

   `(magit-tag
     ((t (:foreground ,yellow
                      :weight bold))))

   `(magit-refname
     ((t (:foreground ,cyan))))

   `(magit-refname-stash
     ((t (:foreground ,magenta))))

   `(magit-refname-wip
     ((t (:foreground ,magenta))))

   `(magit-diff-context
     ((t (:foreground ,fg-dim
                      :background ,bg))))

   `(magit-diff-context-highlight
     ((t (:foreground ,fg-muted
                      :background ,bg-highlight))))

   `(magit-diff-added
     ((t (:foreground ,green
                      :background ,green-bg))))

   `(magit-diff-added-highlight
     ((t (:foreground ,green
                      :background ,green-bg-hl
                      :weight bold))))

   `(magit-diff-added-over
     ((t (:foreground ,green
                      :background ,green-bg-hl))))

   `(magit-diff-removed
     ((t (:foreground ,red-bright
                      :background ,red-bg))))

   `(magit-diff-removed-highlight
     ((t (:foreground ,red
                      :background ,red-bg-hl
                      :weight bold))))

   `(magit-diff-removed-over
     ((t (:foreground ,red-bright
                      :background ,red-bg-hl))))

   `(magit-diff-file-heading
     ((t (:foreground ,blue
                      :weight bold))))

   `(magit-diff-file-heading-highlight
     ((t (:foreground ,cyan
                      :background ,bg-highlight
                      :weight bold))))

   `(magit-diff-hunk-heading
     ((t (:foreground ,blue
                      :background ,bg-alt))))

   `(magit-diff-hunk-heading-highlight
     ((t (:foreground ,cyan
                      :background ,bg-highlight
                      :weight bold))))

   `(magit-process-ok
     ((t (:foreground ,green
                      :weight bold))))

   `(magit-process-ng
     ((t (:foreground ,red
                      :weight bold))))

   `(magit-process-unpushed
     ((t (:foreground ,yellow))))

   `(magit-process-unpulled
     ((t (:foreground ,blue))))

   `(magit-dimmed
     ((t (:foreground ,fg-dim))))

   `(magit-sequence
     ((t (:foreground ,magenta))))

   `(magit-sequence-done
     ((t (:foreground ,green))))

   `(magit-sequence-drop
     ((t (:foreground ,red))))

   `(magit-sequence-head
     ((t (:foreground ,blue
                      :weight bold))))

   `(magit-sequence-part
     ((t (:foreground ,yellow))))

   `(magit-sequence-stop
     ((t (:foreground ,red
                      :weight bold))))

   `(magit-bisect-good
     ((t (:foreground ,green))))

   `(magit-bisect-bad
     ((t (:foreground ,red))))

   `(magit-bisect-skip
     ((t (:foreground ,yellow))))

   `(magit-signature-good
     ((t (:foreground ,green))))

   `(magit-signature-bad
     ((t (:foreground ,red))))

   `(magit-signature-untrusted
     ((t (:foreground ,yellow))))

   `(transient-heading
     ((t (:foreground ,blue
                      :weight bold))))

   `(transient-key
     ((t (:foreground ,cyan
                      :weight bold))))

   `(transient-value
     ((t (:foreground ,fg))))

   `(transient-argument
     ((t (:foreground ,yellow))))

   `(transient-inactive-value
     ((t (:foreground ,fg-dim))))

   `(transient-inactive-argument
     ((t (:foreground ,fg-dim))))

   `(dirvish-hl-line
     ((t (:background ,bg-highlight))))

   `(dirvish-path-separators
     ((t (:foreground ,fg-guide))))

   `(dirvish-directory
     ((t (:foreground ,blue
                      :weight bold))))

   `(dirvish-symlink
     ((t (:foreground ,cyan))))

   `(dirvish-file
     ((t (:foreground ,fg))))

   `(dirvish-subtree-guide
     ((t (:foreground ,fg-guide))))

   `(dirvish-quick-access
     ((t (:foreground ,blue-bright
                      :weight bold))))

   `(dirvish-header
     ((t (:background ,bg-alt
                      :foreground ,blue
                      :weight bold))))

   `(dirvish-header-line
     ((t (:background ,bg-alt
                      :foreground ,fg))))

   `(dirvish-side
     ((t (:background ,bg
                      :foreground ,fg))))

   `(dirvish-side-header
     ((t (:background ,bg-alt
                      :foreground ,blue
                      :weight bold))))

   `(dirvish-collapse
     ((t (:foreground ,fg-muted))))

   `(dired-directory
     ((t (:foreground ,blue
                      :weight bold))))

   `(dired-flagged
     ((t (:foreground ,red
                      :weight bold))))

   `(dired-header
     ((t (:foreground ,blue
                      :weight bold))))

   `(dired-ignored
     ((t (:foreground ,fg-dim))))

   `(dired-mark
     ((t (:foreground ,yellow
                      :weight bold))))

   `(dired-marked
     ((t (:foreground ,yellow
                      :weight bold))))

   `(dired-perm-write
     ((t (:foreground ,green))))

   `(dired-symlink
     ((t (:foreground ,cyan))))

   `(dired-warning
     ((t (:foreground ,red
                      :weight bold))))

   `(compilation-error
     ((t (:foreground ,red
                      :weight bold))))

   `(compilation-warning
     ((t (:foreground ,yellow
                      :weight bold))))

   `(compilation-info
     ((t (:foreground ,green))))

   `(compilation-line-number
     ((t (:foreground ,fg-dim))))

   `(compilation-column-number
     ((t (:foreground ,fg-muted))))

   `(grep-match
     ((t (:background ,bg-selection
                      :foreground ,fg-bright
                      :weight bold))))

   `(success
     ((t (:foreground ,green
                      :weight bold))))

   `(warning
     ((t (:foreground ,yellow
                      :weight bold))))

   `(error
     ((t (:foreground ,red
                      :weight bold))))

   `(info
     ((t (:foreground ,blue))))

   `(completions-highlight
     ((t (:background ,bg-highlight
                      :foreground ,cyan
                      :weight bold))))

   `(vertico-current
     ((t (:background ,bg-highlight
                      :foreground ,cyan
                      :weight bold))))

   `(orderless-match-face-0
     ((t (:foreground ,blue
                      :weight bold))))

   `(orderless-match-face-1
     ((t (:foreground ,green
                      :weight bold))))

   `(orderless-match-face-2
     ((t (:foreground ,yellow
                      :weight bold))))

   `(orderless-match-face-3
     ((t (:foreground ,magenta
                      :weight bold))))

   `(tab-bar
     ((t (:background ,bg
                      :foreground ,fg-dim))))

   `(tab-bar-tab
     ((t (:background ,bg-alt
                      :foreground ,blue
                      :weight bold))))

   `(tab-bar-tab-inactive
     ((t (:background ,bg
                      :foreground ,fg-dim))))

   `(help-key-binding
     ((t (:foreground ,cyan
                      :weight bold))))

   `(widget-field
     ((t (:background ,bg-highlight
                      :foreground ,fg
                      :box (:line-width 1
                             :color ,fg-guide)))))

   `(widget-button
     ((t (:foreground ,blue
                      :weight bold))))

   `(widget-button-pressed
     ((t (:foreground ,green
                      :weight bold))))

   `(button
     ((t (:foreground ,cyan
                      :underline t))))

   `(tooltip
     ((t (:background ,bg-alt
                      :foreground ,fg))))

   `(show-paren-match
     ((t (:background ,bg-selection
                      :foreground ,fg-bright
                      :weight bold))))

   `(show-paren-mismatch
     ((t (:background ,red
                      :foreground ,fg-bright
                      :weight bold))))
   ))

(provide-theme 'debian-i3)

;;; debian-i3.el ends here
