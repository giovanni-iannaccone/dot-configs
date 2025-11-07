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
   irony
   doom-themes
   eglot))

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

(defun search-selection-on-internet (&optional keywords)
  (interactive)
  (let ((selected-text (buffer-substring (region-beginning) (region-end))))
    (eww (format "%s" selected-text ))))

(defun mark-whole-line ()
  (interactive)
  (beginning-of-line)
  (push-mark nil nil 1)
  (end-of-line))

;; Key binding

(global-set-key (kbd "C-l") 'mark-whole-line)

(global-set-key (kbd "C-c") 'kill-ring-save)
(global-set-key (kbd "C-v") 'yank)

(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "<f5>") 'search-selection-on-internet)

;; Appereance

(setq column-number-mode t)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq c-basic-indent 4)

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

;; Delete the selected text upon text insertion

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; doom-dark+ theme

(custom-set-variables
 '(custom-enabled-themes '(doom-dark+))
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     default))
 '(package-selected-packages
   '(auto-complete-c-headers company-irony doom-themes evil go-mode
                             irony-eldoc nerd-icons-completion
                             nerd-icons-corfu nerd-icons-dired
                             rust-mode smex)))
(custom-set-faces)

;; Eglot

(require 'eglot)

(use-package eglot
  :custom (eglot-ignored-server-capabilities 
           '(:documentOnTypeFormattingProvider)))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'javascript-mode-hook 'eglot-ensure)
