(require 'ansi-color)
(require 'seq)
(require 'subr-x)

(defvar pwnit-target nil
  "Currently selected ELF binary.")

(defun pwnit/target ()
  (or pwnit-target
      (setq pwnit-target (find-elf))))

(defun find-elf ()
  (let* ((root default-directory)
         (current (buffer-file-name))
         (current-elf
          (when (and current (file-executable-p current))
            (when (string-match-p
                   "ELF"
                   (shell-command-to-string
                    (format "file -b %s"
                            (shell-quote-argument current))))
              current)))
         (output
          (unless current-elf
            (shell-command-to-string
             (format "find %s -type f -executable -exec file {} +"
                     (shell-quote-argument root)))))
         (elfs
          (when output
            (seq-filter
             (lambda (line)
               (string-match-p ":.*ELF" line))
             (split-string output "\n" t))))
         (files
          (mapcar
           (lambda (line)
             (car (split-string line ": ")))
           elfs)))
    (or current-elf
        (cond
         ((null files)
          (user-error "No ELF found in %s" root))
         ((= (length files) 1)
          (car files))
         (t
          (completing-read "Select ELF: " files nil t))))))

(defun pwnit/check ()
  (interactive)
  (let* ((elf (pwnit/target))
         (output
          (shell-command-to-string
           (concat "pwnit check "
                   (shell-quote-argument elf))))
         (colored (ansi-color-apply output))
         (pos 0))

    (while (< pos (length colored))
      (let* ((next (next-property-change pos colored))
             (next (or next (length colored)))
             (font-face (get-text-property pos 'font-lock-face colored)))
        (when font-face
          (put-text-property pos next 'face font-face colored))
        (setq pos next)))

    (minibuffer-message "%s" (string-trim-right colored))))

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
(global-set-key (kbd "C-c s") #'pwnit/start)

(provide 'pwn)
