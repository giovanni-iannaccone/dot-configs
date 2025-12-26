;; Melpa

(require 'package)
(add-to-list 'package-archives
	    '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Packages

(defun ensure-package-installed (packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (when (y-or-n-p (format "Package %s is missing. Install it? " package))
        (package-install package)))))

(ensure-package-installed 
 '(company 
   eglot
   flycheck
   irony
   magit
   rust-mode
   yasnippet))

;; Utility functions

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

;; Delete the selected text upon text insertion

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Key binding

(global-set-key (kbd "C-l") 'mark-whole-line)

(global-set-key (kbd "C-c") 'kill-ring-save)
(global-set-key (kbd "C-v") 'yank)

(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(global-set-key (kbd "M-x") 'smex)

;; Appereance

(load-theme 'debian-i3 t)

(setq column-number-mode t)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq c-basic-indent 4)

(set-frame-font "JetBrainsMono NF 13" nil t)

(menu-bar-mode 0)
(tool-bar-mode 0)

(global-display-line-numbers-mode)

(ido-mode 1)

;; Company

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil))

;; Rust

(use-package rust-ts-mode
  :mode ("\\.rs" . rust-ts-mode)
  :hook ((rust-ts-mode . eglot-ensure)
         (rust-ts-mode . company-mode))
  :config
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer"))))

;; Irony

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

(require 'irony)

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'rust-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'company-mode))

;; variables

(custom-set-variables
 '(custom-safe-themes
   '("900f067cd6d9e7ef8104b2eea3be9a4dc703936bc91a6e1380eeaa3fb7b8eec1"
     "abb7432b521577a7ed6f39494a2693e95660c095b287346ac08f54cdbf04f513"
     "e4534f696824122b2f14bc135bd7cfddf9f6899c7341e85632fef1e3ae4f1b8b"
     "9af6c155bdc4ced2c070d106c4b6e2bff8927fd2b73de172c893f2167951609d"
     "30e86ff71b48ace65d107369c866f72a8026f9aee9d51af14027dc1f592a8389"
     "3aca0ba9406433384f04c547defb40aa5d7f73ccf7440f3651125bd94affef52"
     "67969b721d540195a76904b0a690d22b22b934991d6c552c5550cdc2da86c2e4"
     "a9805d705c6b3348493d5679d835426efdf458da7de3d3815aae56046745138e"
     default))
 '(package-selected-packages
   '(auto-complete-c-headers company-irony doom-themes flycheck go-mode
                             goto-chg irony-eldoc lsp-ui magit
                             nerd-icons-completion nerd-icons-corfu
                             nerd-icons-dired projectile rust-mode
                             smex yasnippet)))
(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#4e5b55")))))

;; Eglot

(require 'eglot)

(use-package eglot
  :custom (eglot-ignored-server-capabilities 
           '(:documentOnTypeFormattingProvider)))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'javascript-mode-hook 'eglot-ensure)
