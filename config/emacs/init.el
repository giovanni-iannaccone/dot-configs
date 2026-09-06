(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t
      use-package-always-defer t)

(setq inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      make-backup-files nil
      use-file-dialog nil
      column-number-mode t)

(setq-default indent-tabs-mode nil
              make-backup-files nil
              cursor-type 'bar 
              tab-width 4
              c-basic-offset 4
              c-basic-indent 4)

(set-frame-font "JetBrainsMono NF 13" nil t)

(blink-cursor-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(winner-mode 1)

(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(server-start)

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun mark-whole-line ()
  (interactive)
  (beginning-of-line)
  (push-mark nil nil 1)
  (end-of-line))

(defun my-search-selection (start end fn)
  "Search selected text with FN."
  (interactive "r")
  (let ((text (string-trim
               (buffer-substring-no-properties start end))))
    (when (string-empty-p text)
      (user-error "No text selected"))
    (funcall fn
             (if (string-match-p "^\\(http\\|https\\|file\\|ftp\\):" text)
                 text
               (concat "https://lite.duckduckgo.com/lite/?q="
                       (url-hexify-string text))))))

(defun search-selection-with-eww (start end)
  (interactive "r")
  (my-search-selection start end #'eww))

(defun search-selection-with-browser (start end)
  (interactive "r")
  (my-search-selection start end #'browse-url))

(delete-selection-mode 1)

(global-set-key (kbd "C-l") #'mark-whole-line)
(global-set-key (kbd "C-v") #'yank)
(global-set-key (kbd "C-z") #'undo)

(global-set-key (kbd "M-<up>") #'move-line-up)
(global-set-key (kbd "M-<down>") #'move-line-down)

(global-set-key (kbd "<f5>") #'search-selection-with-eww)
(global-set-key (kbd "C-<f5>") #'search-selection-with-browser)

(global-set-key (kbd "C-M-w") #'woman)

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package isearch
  :ensure nil
  :custom
  (search-whitespace-regexp ".*?")
  (isearch-lazy-count t)
  (isearch-wrap-pause 'no)
  :bind
  (:map isearch-mode-map
        ("<down>" . isearch-repeat-forward)
        ("<up>" . isearch-repeat-backward)))

(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("M-g g" . consult-goto-line)
   ("M-s l" . consult-line)
   ("M-s r" . consult-ripgrep)
   ("M-s f" . consult-find)
   ("M-y" . consult-yank-pop)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  :init
  (global-corfu-mode 1))

(use-package cape
  :bind
  ("C-c p" . cape-prefix-map))

(use-package eglot
  :hook
  ((c-mode
    c++-mode
    go-mode
    python-mode
    python-ts-mode)
   . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities
   '(:documentOnTypeFormattingProvider))
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio")))
(setq-default
 eglot-workspace-configuration
 '((:basedpyright
    . ((analysis
        . ((typeCheckingMode . "basic")
           (diagnosticSeverityOverrides
            . ((reportUnknownVariableType . "none")
               (reportUnknownMemberType . "none")
               (reportUnknownArgumentType . "none")
               (reportOptionalMemberAccess . "none")
               (reportOperatorIssue . "none")
               (reportWildcardImportFromLibrary . "none")
               (reportAttributeAccessIssue . "none")
               (reportUnknownParameterType . "none")))))))))

(use-package magit
  :defer t
  :bind
  ("C-x g" . magit-status))

(use-package project
  :ensure nil
  :bind-keymap
  ("C-x p" . project-prefix-map))

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1))

(use-package autorevert
  :ensure nil
  :init
  (global-auto-revert-mode 1))

(use-package dirvish
  :init
  (dirvish-override-dired-mode 1)
  :bind
  ("C-x d" . dirvish))

(defun my-vterm-toggle ()
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (previous-buffer)
    (vterm)))

(use-package vterm
  :bind
  ("C-x t" . my-vterm-toggle)
  :custom
  (vterm-shell "/bin/zsh")
  (vterm-max-scrollback 1000)
  (vterm-timer-delay 0.01))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package pdf-tools
  :commands (pdf-tools-install)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package proced
  :ensure nil
  :defer t
  :commands proced
  :bind (("C-M-p" . proced))
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200)
  :bind
  ("C-x C-r" . #'consult-recent-file))

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh"))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c m m" . mc/mark-more-like-this-extended)
   ("C-c m l" . mc/mark-lines)))

(load-theme 'debian-i3 t)

(set-frame-parameter nil 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80))

(unless (display-graphic-p)
  (set-face-background 'default "unspecified"))
