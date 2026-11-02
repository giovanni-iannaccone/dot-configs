(load-theme 'debian-i3 t)

(add-to-list 'load-path "~/.config/emacs/lisp")
(require 'modeline)

(menu-bar-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq default-frame-alist
      '((alpha-background . 85)
        (font . "JetBrainsMono NF-13")))

(setq use-package-always-ensure t
      use-package-always-defer t
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      use-file-dialog nil
      column-number-mode t)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
