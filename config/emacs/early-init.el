(load-theme 'debian-i3 t)

(menu-bar-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(add-to-list 'default-frame-alist '(alpha-background . 85))

(setq use-package-always-ensure t
      use-package-always-defer t
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      use-file-dialog nil
      column-number-mode t)
