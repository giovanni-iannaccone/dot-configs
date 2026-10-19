(require 'cl-lib)
(require 'subr-x)

(defgroup my/org nil "Personal Org workflow." :group 'org)

(defcustom my/org-directory
  (expand-file-name "~/Documents/org/")
  "Org directory."
  :type 'directory
  :group 'my/org)

(defvar my/org-archive-directory
  (expand-file-name "archive/" my/org-directory))

(defun my/org-after-save-refresh-agenda ()
  "Refresh Org agenda files after saving an Org buffer."
  (when (derived-mode-p 'org-mode)
    (my/org-refresh-agenda-files)))

(defun my/org-refresh-agenda-files ()
    "Recursively collect all Org files under `org-directory`."
    (interactive)
    (setq org-agenda-files
          (directory-files-recursively
           my/org-directory
           "\\.org\\'")))

  (setq org-support-shift-select t)  
  (my/org-refresh-agenda-files)

(defun my/org-open-inbox ()
  (interactive)
  (find-file org-default-notes-file))

(defun my/org-agenda-today ()
  (interactive)
  (org-agenda nil "d"))

(defun my/org-setup-buffer ()
  (setq-local line-spacing 0.12))

(use-package org
  :ensure nil
  :init
  (setq org-directory (file-name-as-directory (expand-file-name my/org-directory))
        org-default-notes-file (expand-file-name "inbox.org" my/org-directory)
        org-archive-location (expand-file-name "archive/%s_archive::datetree" my/org-directory))
  (make-directory org-directory t)
  (make-directory my/org-archive-directory t)
  (my/org-refresh-agenda-files)

  :custom
  (org-support-shift-select t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAIT(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "#00B7FF" :weight bold))
     ("NEXT" . (:foreground "#00E5FF" :weight bold))
     ("IN-PROGRESS" . (:foreground "#FFB86C" :weight bold))
     ("WAIT" . (:foreground "#FFD600" :weight bold))
     ("DONE" . (:foreground "#00CC00"))
     ("CANCELLED" . (:foreground "#607080"))))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-priority-default ?B)
  (org-priority-lowest ?E)
  (org-priority-highest ?A)
  (org-priority-faces
   '((?A . (:foreground "#FF0033" :weight bold))
     (?B . (:foreground "#FFD600" :weight bold))
     (?C . (:foreground "#00B7FF"))
     (?D . (:foreground "#AAB6C5"))
     (?E . (:foreground "#607080"))))
  (org-tag-alist
   '((:startgroup)
     ("university" . ?u)
     ("project" . ?p)
     ("ctf" . ?c)
     ("personal" . ?h)
     ("work" . ?w)
     (:endgroup)
     (:startgroup)
     ("solve" . ?s)
     ("fix" . ?f)
     ("build" . ?b)
     ("learn" . ?l)
     ("read" . ?r)
     ("write" . ?W)
     ("research" . ?R)
     ("review" . ?v)
     ("organize" . ?o)
     ("other" . ?O)
     ("idea" . ?I)
     (:endgroup)))
  (org-agenda-span 7)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-with-log-mode nil)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-show-future-repeats nil)
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-tags-column -80)
  (org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     "......" "----------------"))
  (org-agenda-custom-commands
   '(("d" "Daily dashboard"
      ((agenda "" ((org-agenda-span 1)
                   (org-agenda-overriding-header "Today")))
       (todo "IN-PROGRESS" ((org-agenda-overriding-header "In progress")))
       (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
       (todo "WAIT" ((org-agenda-overriding-header "Waiting for")))))
     ("w" "Weekly overview"
      ((agenda "" ((org-agenda-span 7)
                   (org-agenda-overriding-header "This week")))
       (todo "IN-PROGRESS" ((org-agenda-overriding-header "In progress")))
       (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
       (agenda "" ((org-agenda-entry-types '(:deadline))
                   (org-agenda-span 30)
                   (org-agenda-overriding-header "Upcoming deadlines")))))
     ("t" "Tasks"
      ((todo "TODO" ((org-agenda-overriding-header "Todo")))
       (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
       (todo "IN-PROGRESS" ((org-agenda-overriding-header "In progress")))
       (todo "WAIT" ((org-agenda-overriding-header "Waiting for")))))
     ("u" "University"
      ((tags-todo "university" ((org-agenda-overriding-header "University")))))
     ("p" "Projects"
      ((tags-todo "project" ((org-agenda-overriding-header "Projects")))))
     ("c" "CTF"
      ((tags-todo "ctf" ((org-agenda-overriding-header "CTF")))))
     ("i" "Important"
      ((todo "TODO|NEXT|IN-PROGRESS|WAIT"
             ((org-agenda-overriding-header "High priority tasks")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'notregexp "\\[#A\\]"))))))))

  (org-capture-templates
   '(("t" "Task" entry (file org-default-notes-file)
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)
     ("d" "Task with deadline" entry (file org-default-notes-file)
      "* TODO %?\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)
     ("n" "Note" entry (file org-default-notes-file)
      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)
     ("i" "Idea" entry (file org-default-notes-file)
      "* %? :idea:\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)
     ("u" "University task" entry (file org-default-notes-file)
      "* TODO %? :university:\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)
     ("p" "Project task" entry (file org-default-notes-file)
      "* TODO %? :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)
     ("c" "CTF task" entry (file org-default-notes-file)
      "* TODO %? :ctf:\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)
     ("a" "Appointment" entry (file org-default-notes-file)
      "* %?\n%^T\n"
      :empty-lines 1)))

  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-clock-persist 'history)
  (org-clock-in-resume t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-mode-line-total 'today)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " …")
  (org-preview-latex-default-process 'dvisvgm)
  (org-return-follows-link t)
  (org-id-link-to-org-use-id 'create)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-fontify-quote-and-verse-blocks t)
  (org-cycle-separator-lines 2)
  (org-startup-with-inline-images nil)
  (org-image-actual-width '(600))

  :config
  (org-clock-persistence-insinuate)
  (add-hook 'after-save-hook #'my/org-after-save-refresh-agenda)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'my/org-setup-buffer))

(use-package org-modern
  :ensure t
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-star nil)
  (org-modern-list '((43 . "•") (45 . "–") (42 . "•")))
  (org-modern-checkbox '((?X . "☑") (?- . "◩") (?\s . "☐")))
  (org-modern-todo t)
  (org-modern-tag nil)
  (org-modern-priority t)
  (org-modern-timestamp t)
  (org-modern-statistics t)
  (org-modern-progress t)
  (org-modern-horizontal-rule t)
  (org-modern-keyword t)
  (org-modern-block-name t)
  (org-modern-table t))

(with-eval-after-load 'org
    (custom-set-faces
     '(org-level-1 ((t (:foreground "#00B7FF" :weight bold :height 1.30))))
     '(org-level-2 ((t (:foreground "#D8DEE9" :weight bold :height 1.15))))
     '(org-level-3 ((t (:foreground "#D8DEE9" :weight medium :height 1.05))))
     '(org-level-4 ((t (:foreground "#AAB6C5" :weight medium))))
     '(org-level-5 ((t (:foreground "#AAB6C5"))))
     '(org-level-6 ((t (:foreground "#AAB6C5"))))
     '(org-level-7 ((t (:foreground "#AAB6C5"))))
     '(org-level-8 ((t (:foreground "#AAB6C5"))))
     '(org-document-title ((t (:foreground "#FFFFFF" :weight bold :height 1.5))))
     '(org-block ((t (:background "#0A0A0A" :extend t))))
     '(org-block-begin-line ((t (:foreground "#607080" :background "#0A0A0A" :extend t))))
     '(org-block-end-line ((t (:foreground "#607080" :background "#0A0A0A" :extend t))))
     '(org-code ((t (:foreground "#00E5FF"))))
     '(org-verbatim ((t (:foreground "#00E5FF"))))
     '(org-table ((t (:foreground "#D8DEE9"))))
     '(org-meta-line ((t (:foreground "#607080"))))
     '(org-special-keyword ((t (:foreground "#607080"))))
     '(org-drawer ((t (:foreground "#607080"))))
     '(org-property-value ((t (:foreground "#AAB6C5"))))
     '(org-date ((t (:foreground "#00B7FF" :underline nil))))
     '(org-link ((t (:foreground "#00E5FF" :underline nil))))
     '(org-tag ((t (:foreground "#607080" :weight normal))))
     '(org-todo ((t (:foreground "#00B7FF" :weight bold))))
     '(org-done ((t (:foreground "#00CC00" :weight normal))))
     '(org-headline-done ((t (:foreground "#607080" :weight normal))))
     '(org-checkbox ((t (:foreground "#607080"))))
     '(org-list-dt ((t (:foreground "#D8DEE9" :weight medium))))
     '(org-quote ((t (:foreground "#607080" :slant italic :extend t))))
     '(org-verse ((t (:foreground "#D8DEE9" :slant italic))))
     '(org-agenda-structure ((t (:foreground "#00B7FF" :weight bold))))
     '(org-agenda-date ((t (:foreground "#D8DEE9" :weight medium))))
     '(org-agenda-date-today ((t (:foreground "#00E5FF" :weight bold))))
     '(org-agenda-date-weekend ((t (:foreground "#607080"))))
     '(org-agenda-done ((t (:foreground "#607080"))))
     '(org-scheduled ((t (:foreground "#D8DEE9"))))
     '(org-scheduled-today ((t (:foreground "#00B7FF" :weight medium))))
     '(org-upcoming-deadline ((t (:foreground "#FFD600"))))
     '(org-warning ((t (:foreground "#FF0033" :weight bold))))
     '(org-modern-label ((t (:inherit default)))))
  
  (dolist (face '(org-block org-block-begin-line org-block-end-line
                            org-code org-verbatim org-table))
    (set-face-attribute face nil :inherit 'fixed-pitch)))

(define-key global-map (kbd "C-c a") #'org-agenda)
(define-key global-map (kbd "C-c c") #'org-capture)
(define-key global-map (kbd "C-c l") #'org-store-link)
(define-key global-map (kbd "C-c o") #'org-open-at-point)
(define-key global-map (kbd "C-c n i") #'my/org-open-inbox)
(define-key global-map (kbd "C-c n d") #'my/org-agenda-today)
(define-key global-map (kbd "C-c n r") #'my/org-refresh-agenda-files)

(provide 'orgconfig)
