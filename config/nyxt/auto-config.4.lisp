(define-configuration (input-buffer)
  ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-configuration browser
  ((theme theme:+dark-theme+)))

(define-configuration (web-buffer)
  ((default-modes (pushnew 'nyxt/mode/style:dark-mode %slot-value%))))

(define-configuration (input-buffer)
  ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-configuration (input-buffer)
  ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-configuration (web-buffer)
  ((default-modes
    (remove-if (lambda (nyxt::m) (string= (symbol-name nyxt::m) "DARK-MODE"))
               %slot-value%))))

(defmethod customize-instance ((buffer buffer) &key)
  (setf (slot-value buffer 'default-modes)
          '(nyxt/mode/emacs:emacs-mode nyxt/mode/autofill:autofill-mode
                                       nyxt/mode/spell-check:spell-check-mode
                                       nyxt/mode/search-buffer:search-buffer-mode
                                       nyxt/mode/hint:hint-mode
                                       nyxt/mode/document:document-mode
                                       nyxt/mode/password:password-mode
                                       nyxt/mode/bookmark:bookmark-mode
                                       nyxt/mode/annotate:annotate-mode
                                       base-mode)))
