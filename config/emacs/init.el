(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(unless (require 'use-package nil t)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

(setq yank-excluded-properties
      '(category field mouse-face))

(setq native-comp-async-report-warnings-errors 'silent 
      sentence-end-double-space nil
      require-final-newline t
      scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position t
      enable-recursive-minibuffers t
      display-line-numbers-type 'relative)

(setq-default indent-tabs-mode nil
              make-backup-files nil
              tab-width 4
              c-basic-offset 4
              c-basic-indent 4)

(blink-cursor-mode -1)
(winner-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(fset 'yes-or-no-p 'y-or-n-p)
(defvaralias 'woman-topic-history 'Man-topic-history)

(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

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

(global-set-key (kbd "C-l") #'mark-whole-line)
(global-set-key (kbd "C-v") #'yank)

(global-set-key (kbd "<f5>") #'search-selection-with-eww)
(global-set-key (kbd "C-<f5>") #'search-selection-with-browser)

(global-set-key (kbd "C-c k") #'kill-matching-buffers-no-ask)

(global-set-key (kbd "C-<tab>") #'switch-to-next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") #'switch-to-prev-buffer)

(global-set-key (kbd "C-M-w") #'woman)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c <left>")  #'winner-undo)
(global-set-key (kbd "C-c <right>") #'winner-redo)

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

(use-package undo-fu
  :bind
  (("C-z"       . undo-fu-only-undo)
   ("C-S-z"     . undo-fu-only-redo)))

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
  (("C-x b"   . consult-buffer)
   ("C-x C-r" . consult-recent-file)
   ("C-c i"   . consult-imenu)
   ("M-g g"   . consult-goto-line)
   ("M-s l"   . consult-line)
   ("M-s r"   . consult-ripgrep)
   ("M-s f"   . consult-find)
   ("M-y"     . consult-yank-pop)))

(use-package move-text
  :ensure t
  :bind
  (("M-<up>"   . move-text-up)
   ("M-<down>" . move-text-down)))

(use-package ace-window
  :bind
  (("M-o" . ace-window))
  :custom
  (aw-dispatch-always t)
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-use-other-window nil)
  (ibuffer-default-sorting-mode 'recency)
  (ibuffer-show-empty-filter-groups nil))

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

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)))

(use-package ace-link
  :init
  (ace-link-setup-default))

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

(with-eval-after-load 'eglot
  (set-face-attribute 'eglot-inlay-hint-face nil
                      :height 1.0))

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
  :ensure t
  :init
  (dirvish-override-dired-mode 1)
  :bind
  ("C-x d" . dirvish))

(defun my-eat-toggle ()
  (interactive)
  (if (eq major-mode 'eat-mode)
      (previous-buffer)
    (eat)))

(use-package eat
  :ensure t
  :custom
  (shell-file-name "/bin/zsh")
  (eat-shell "/bin/zsh")
  (eat-enable-directory-tracking t)
  (eat-shell-prompt-annotation-success-margin-indicator "✔")
  (eat-term-scrollback-size (* 100 1024))
  :bind
  ("C-x t" . my-eat-toggle))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

(use-package proced
  :ensure nil
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
  (recentf-max-saved-items 200))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :custom
  (yas-snippet-dirs '("~/.config/emacs/snippets"))
  (yas-indent-line 'fixed)
  :config
  (yas-reload-all))

(with-eval-after-load 'woman
  (setq woman-fontify t
        woman-bold-headings t
        woman-fill-frame t)
  
  (set-face-attribute 'woman-bold nil
                      :inherit 'bold
                      :foreground "#00A8FF")
  
  (set-face-attribute 'woman-italic nil
                      :inherit 'italic
                      :foreground "#00CC00"))

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh"))

(with-eval-after-load 'tramp
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process))

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c m m" . mc/mark-more-like-this-extended)
   ("C-c m l" . mc/mark-lines)))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package ctftime
  :ensure t)

(unless (display-graphic-p)
  (set-face-background 'default "unspecified"))

(require 'orgconfig)
(require 'pwn)
