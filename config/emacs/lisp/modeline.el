(defgroup my/modeline nil
  "Minimal modeline."
  :group 'mode-line)

(defcustom my/modeline-enable-icons t
  "Whether to display Nerd Icons."
  :type 'boolean
  :group 'my/modeline)

(defface my/modeline-accent
  '((t (:foreground "#00B7FF" :weight bold)))
  "Modeline accent face.")

(defface my/modeline-muted
  '((t (:foreground "#607080")))
  "Modeline muted face.")

(defface my/modeline-success
  '((t (:foreground "#00CC00")))
  "Modeline success face.")

(defface my/modeline-warning
  '((t (:foreground "#FFD600" :weight bold)))
  "Modeline warning face.")

(defface my/modeline-error
  '((t (:foreground "#FF0033" :weight bold)))
  "Modeline error face.")

(with-eval-after-load 'flymake
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception
          flymake-mode-line-counters)
        flymake-mode-line-counter-format
        '("" flymake-mode-line-error-counter
          " "
          flymake-mode-line-warning-counter
          ""))

  (when (boundp 'flymake-suppress-zero-counters)
    (setq flymake-suppress-zero-counters nil))

  (set-face-attribute 'flymake-error nil
                      :foreground "#FF0033"
                      :weight 'bold)

  (set-face-attribute 'flymake-warning nil
                      :foreground "#FFD600"
                      :weight 'bold))

(defun my/modeline--space ()
  "Return a modeline space."
  #(" " 0 1 (face mode-line)))

(defun my/modeline--gap ()
  "Return a modeline separator."
  #("   " 0 3 (face mode-line)))

(defun my/modeline--face (string face)
  "Apply FACE to STRING."
  (when (and string (not (string-empty-p string)))
    (propertize string 'face face)))

(defun my/modeline--format (format face)
  "Format FORMAT and apply FACE."
  (my/modeline--face (format-mode-line format) face))

(defun my/modeline-buffer ()
  "Display current buffer name."
  (my/modeline--face (buffer-name) 'mode-line-buffer-id))

(defun my/modeline-mode ()
  "Display current major mode."
  (my/modeline--format mode-name 'my/modeline-muted))

(defun my/modeline-icon ()
  "Display an icon for the current buffer."
  (when (and my/modeline-enable-icons
             (display-graphic-p)
             (require 'nerd-icons nil t))
    (condition-case nil
        (nerd-icons-icon-for-buffer)
      (error nil))))

(defvar-local my/modeline-git-branch nil
  "Cached Git branch.")

(defvar-local my/modeline-git-last-refresh 0
  "Time of the last Git branch refresh.")

(defun my/modeline-refresh-git (&rest _)
  "Refresh cached Git branch."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name)))
    (let ((root (locate-dominating-file default-directory ".git")))
      (setq my/modeline-git-branch
            (when root
              (with-temp-buffer
                (let ((default-directory root))
                  (when (zerop
                         (process-file "git" nil t nil
                                       "symbolic-ref" "--quiet"
                                       "--short" "HEAD"))
                    (string-trim (buffer-string)))))))))

  (force-mode-line-update))

(defun my/modeline-git ()
  "Display current Git branch."
  (when my/modeline-git-branch
    (concat
     (my/modeline--face " " 'my/modeline-accent)
     (my/modeline--face my/modeline-git-branch
                        'my/modeline-accent))))

(defun my/modeline-eglot ()
  "Display Eglot status."
  (when (bound-and-true-p eglot--managed-mode)
    (my/modeline--face "󰒋 Eglot" 'my/modeline-accent)))

(defun my/modeline-position ()
  "Display cursor position."
  (my/modeline--format "%l:%c" 'my/modeline-muted))

(defun my/modeline-percent ()
  "Display buffer position percentage."
  (my/modeline--format "%p" 'my/modeline-accent))

(defun my/modeline-left ()
  "Return left side of the modeline."
  (concat
   (my/modeline--space)
   (or (my/modeline-icon) "")
   (my/modeline--space)
   (my/modeline-buffer)
   (my/modeline--gap)
   (my/modeline-mode)
   (my/modeline--gap)
   (when (bound-and-true-p flymake-mode)
      (format-mode-line flymake-mode-line-format))))

(defun my/modeline-right ()
  "Return right side of the modeline."
  (concat
   (my/modeline-git)
   (my/modeline--gap)
   (my/modeline-eglot)
   (my/modeline--gap)
   (my/modeline-position)
   (my/modeline--space)
   (my/modeline-percent)
   (my/modeline--gap)))

(defun my/modeline-refresh (&rest _)
  "Refresh modeline."
  (force-mode-line-update t))

(defun my/modeline-refresh-all ()
  "Refresh Git information in all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (my/modeline-refresh-git))))

(defun my/modeline-setup-hooks ()
  "Configure modeline hooks."
  (add-hook 'find-file-hook #'my/modeline-refresh-git)
  (add-hook 'dired-mode-hook #'my/modeline-refresh-git)
  (add-hook 'after-save-hook #'my/modeline-refresh-git)
  (add-hook 'vc-post-command-functions #'my/modeline-refresh-git)
  (add-hook 'buffer-list-update-hook #'my/modeline-refresh)

  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook
              #'my/modeline-refresh-all))

  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook
              #'my/modeline-refresh))

  (with-eval-after-load 'flymake
    (add-hook 'flymake-after-diagnostics-hook
              #'my/modeline-refresh)))

(defun my/modeline-setup-faces ()
  "Configure modeline appearance."
  (set-face-attribute 'mode-line nil
                      :background "#0A0A0A"
                      :foreground "#D8DEE9"
                      :box nil
                      :height 1.0)

  (set-face-attribute 'mode-line-inactive nil
                      :background "#000000"
                      :foreground "#607080"
                      :box nil
                      :height 1.0))

(defun my/modeline-init ()
  "Initialize custom modeline."
  (my/modeline-setup-faces)
  (my/modeline-setup-hooks)

  (setq-default
   mode-line-format
   '("%e"
     (:eval (my/modeline-left))
     mode-line-format-right-align
     (:eval (my/modeline-right))))

  (my/modeline-refresh-all))

(my/modeline-init)

(provide 'modeline)
