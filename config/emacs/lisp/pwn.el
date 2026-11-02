(require 'ansi-color)
(require 'seq)
(require 'subr-x)

(defvar pwnit-target nil
  "Currently selected ELF binary.")

(defun pwnit/target ()
  (or pwnit-target
      (setq pwnit-target (find-elf))))

(defun find-elf ()
  (or (and-let* ((file (buffer-file-name))
                 (_ (file-executable-p file))
                 (_ (string-match-p "ELF"
                      (shell-command-to-string
                       (format "file -b %s" (shell-quote-argument file))))))
        file)
      (pcase (seq-keep
              (lambda (s)
                (and (string-match "^\\(.*\\):.*ELF" s)
                     (match-string 1 s)))
              (split-string
               (shell-command-to-string
                (format "find %s -type f -executable -exec file {} +"
                        (shell-quote-argument default-directory)))
               "\n" t))
        (`() (user-error "No ELF found in %s" default-directory))
        (`(,file) file)
        (files (completing-read "Select ELF: " files nil t)))))

(defun pwnit/check ()
  (interactive)
  (let* ((output (ansi-color-apply
                  (shell-command-to-string
                   (concat "pwnit check "
                           (shell-quote-argument (pwnit/target))))))
         (pos 0))
    (while (< pos (length output))
      (let ((next (or (next-property-change pos output)
                      (length output))))
        (when-let ((face (get-text-property pos 'font-lock-face output)))
          (put-text-property pos next 'face face output))
        (setq pos next)))
    
    (message "%s" (string-trim-right output))))

(defun pwnit/check-reset ()
  (interactive)
  (let* ((pwnit-target (setq pwnit-target (find-elf)))
         (output (ansi-color-apply
                  (shell-command-to-string
                   (concat "pwnit check "
                           (shell-quote-argument pwnit-target)))))
         (pos 0))
    
    (while (< pos (length output))
      (let ((next (or (next-property-change pos output)
                      (length output))))
        (when-let ((face (get-text-property pos 'font-lock-face output)))
          (put-text-property pos next 'face face output))
        (setq pos next)))
    
    (message "%s" (string-trim-right output))))

(defun pwnit/start-filter (proc output)
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      
      (with-current-buffer buffer        
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          
          (goto-char (point-max))
          (insert output)
          (ansi-color-apply-on-region
           (process-mark proc)
           (point-max))
          
          (set-marker (process-mark proc) (point-max)))))))

(defun pwnit/start-sentinel (proc event)
  (when (memq (process-status proc) '(exit signal))
    (if (and (eq (process-status proc) 'exit)
             (= (process-exit-status proc) 0))
        
        (let* ((buffer (process-buffer proc))
               (window (process-get proc 'pwnit-window))
               (dir (process-get proc 'pwnit-directory))
               (solve (find-file-noselect
                       (expand-file-name "solve.py" dir))))
          
          (when (window-live-p window)
            (set-window-buffer window solve)
            (delete-other-windows window)
            (select-window window))
          
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
      
      (message "pwnit start failed: %s" (string-trim event)))))

(defun pwnit/start ()
  (interactive)
  (let* ((dir default-directory)
         (buffer (get-buffer-create "*pwnit-start*"))
         (window (split-window-below)))
    (set-window-buffer window buffer)
    (select-window window)
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq default-directory dir)
      (setq-local ansi-color-context nil))
    
    (let ((proc (make-process
                 :name "pwnit-start"
                 :buffer buffer
                 :command '("pwnit" "start")
                 :noquery t
                 :filter #'pwnit/start-filter
                 :sentinel #'pwnit/start-sentinel)))
      
      (process-put proc 'pwnit-directory dir)
      (process-put proc 'pwnit-window window))))

(defun solve/run ()
  (interactive)
  (save-buffer)
  (setq pwn-file (buffer-file-name))
  (eat)
  (run-at-time
   0.1 nil
   (lambda ()
     (when (and (get-buffer "*eat*")
                (buffer-live-p (get-buffer "*eat*")))
       (with-current-buffer "*eat*"
         (eat-term-send-string
          eat-terminal
          (format "python3 -u %s\n"
                  (shell-quote-argument pwn-file))))))))

(defun solve/restart ()
  (interactive)
  (when (and (boundp 'eat-terminal)
             eat-terminal)
    (eat-term-send-string eat-terminal "\C-c")
    (eat-term-send-string
     eat-terminal
     (format "python3 -u %s\n"
             (shell-quote-argument pwn-file)))))

(global-set-key (kbd "C-c d") #'solve/run)
(global-set-key (kbd "C-c D") #'solve/restart)

(global-set-key (kbd "C-c w") #'pwnit/check)
(global-set-key (kbd "C-c W") #'pwnit/check-reset)
(global-set-key (kbd "C-c s") #'pwnit/start)

(provide 'pwn)
