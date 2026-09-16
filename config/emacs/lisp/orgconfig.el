(use-package org
  :ensure nil
  :init
  (setq org-directory (expand-file-name "~/Documents/org/"))

  (defun my/org-refresh-agenda-files ()
    "Recursively collect all Org files under `org-directory`."
    (interactive)
    (setq org-agenda-files
          (directory-files-recursively
           org-directory
           "\\.org\\'")))

  (setq org-support-shift-select t)  
  (my/org-refresh-agenda-files)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "NEXT(n)"
           "WAIT(w@)"
           "IN-PROGRESS"
           "DONE(d!)"
           "CANCELLED(c@)")))

  (setq org-todo-keyword-faces
        '(("TODO"      . org-todo)
          ("NEXT"      . "orange")
          ("IN-PROGRESS" . "blue")
          ("WAIT"      . "magenta")
          ("DONE"      . "green")
          ("CANCELLED" . "gray")))

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-tag-alist
        '((:startgroup)
          ("university" . ?u)
          ("project"    . ?p)
          ("ctf"        . ?c)
          ("personal"   . ?h)
          ("work"       . ?w)
          (:endgroup)

          (:startgroup)
          ("solve"      . ?s)
          ("fix"        . ?f)
          ("build"      . ?b)
          ("learn"      . ?l)
          ("read"       . ?r)
          ("write"      . ?W)
          ("research"   . ?R)
          ("review"     . ?v)
          ("organize"   . ?o)
          ("other"      . ?O)
          (:endgroup)))
  
  (setq org-agenda-span 7)
  (setq org-agenda-start-on-weekday nil)

  (setq org-agenda-start-with-log-mode t)

  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)

  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0800 1000 1200 1400 1600 1800 2000)))

  (setq org-agenda-custom-commands
        '(

          ("d" "Daily dashboard"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-overriding-header
                      "Today")))

            (todo "NEXT"
                  ((org-agenda-overriding-header
                    "Next actions")))

            (todo "WAIT"
                  ((org-agenda-overriding-header
                    "Waiting for")))))

          ("w" "Weekly overview"
           ((agenda ""
                    ((org-agenda-span 7)
                     (org-agenda-overriding-header
                      "Week")))

            (todo "NEXT"
                  ((org-agenda-overriding-header
                    "Next actions")))

            (agenda ""
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-span 30)
                     (org-agenda-overriding-header
                      "Upcoming deadlines")))))

          ("t" "Tasks"
           ((todo "TODO"
                  ((org-agenda-overriding-header
                    "Todo")))

            (todo "NEXT"
                 ((org-agenda-overriding-header
                   "Next actions")))

            (todo "WAIT"
                 ((org-agenda-overriding-header
                   "Waiting for")))))

          ("u" "University"
           ((tags-todo "university"
                       ((org-agenda-overriding-header
                         "University")))))

          ("p" "Projects"
           ((tags-todo "project"
                       ((org-agenda-overriding-header
                         "Projects")))))

          ("c" "CTF"
           ((tags-todo "ctf"
                       ((org-agenda-overriding-header
                         "CTF")))))

          ("i" "Important"
           ((tags-todo "important"
                       ((org-agenda-overriding-header
                         "Important")))))

          ))

  (setq org-default-notes-file
        (expand-file-name "inbox.org" org-directory))

  (setq org-capture-templates
        '(
          ("t" "Task"
           entry
           (file org-default-notes-file)
           "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
           :empty-lines 1)

          ("n" "Note"
           entry
           (file org-default-notes-file)
           "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
           :empty-lines 1)

          ("i" "Idea"
           entry
           (file org-default-notes-file)
           "* TODO %? :idea:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
           :empty-lines 1)

          ("u" "University task"
           entry
           (file org-default-notes-file)
           "* TODO %? :university:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
           :empty-lines 1)

          ("p" "Project task"
           entry
           (file org-default-notes-file)
           "* TODO %? :project:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
           :empty-lines 1)

          ("c" "CTF task"
           entry
           (file org-default-notes-file)
           "* TODO %? :ctf:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
           :empty-lines 1)

          ("a" "Appointment"
           entry
           (file org-default-notes-file)
           "* %?\n%^T\n"
           :empty-lines 1)

          ))

  (setq org-refile-targets
        '((org-agenda-files :maxlevel . 3)))

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-archive-location
        (expand-file-name
         "archive/%s_archive::datetree"
         org-directory))

  (setq org-clock-persist 'history)

  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-ellipsis " ...")

  (setq org-return-follows-link t)

  (setq org-id-link-to-org-use-id 'create)

  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c o" . org-open-at-point))

  :config
  (org-clock-persistence-insinuate)

  (define-key org-mode-map
    (kbd "C-c C-x R")
    #'my/org-refresh-agenda-files)

  (add-hook 'org-mode-hook #'visual-line-mode))

(provide 'orgconfig)
