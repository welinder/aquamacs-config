;; org-mode

(add-to-list 'load-path (concat config-root-dir "/plugins/org-mode/lisp"))
(add-to-list 'load-path (concat config-root-dir "/plugins/org-mode/contrib/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org-install)
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)

(setq org-log-done t)
(setq org-hide-leading-stars t)
(setq org-link-abbrev-alist
       '(("google"   . "http://www.google.com/search?q=")
         ("gmap"     . "http://maps.google.com/maps?q=%s")
         ("omap"     . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")))
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            (make-variable-buffer-local 'yas/trigger-key)
            (org-set-local 'yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)
            ;; flyspell mode for spell checking everywhere
            (flyspell-mode 1)
            ;; auto-fill mode on
            (auto-fill-mode 1)))

(setq org-todo-keywords (quote 
  ((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!/!)")
   (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELLED(c@/!)"))))
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED"
               ("CANCELLED" . t))
              ("WAITING"
               ("WAITING" . t))
              ("SOMEDAY"
               ("WAITING" . t))
              (done
               ("WAITING"))
              ("TODO"
               ("WAITING")
               ("CANCELLED"))
              ("NEXT"
               ("WAITING"))
              ("DONE"
               ("WAITING")
               ("CANCELLED")))))
(setq org-default-notes-file "~/Dropbox/org/refile.org")

;; I use C-M-r to start capture mode
(global-set-key (kbd "C-M-r") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates (quote (
  ("t" "todo" entry (file "~/Dropbox/org/refile.org") "* TODO %?
%U
%a" :clock-in t :clock-resume t)
  ("n" "note" entry (file "~/Dropbox/org/refile.org") "* %?                                                                            :NOTE:
%U
%a
:CLOCK:
:END:" :clock-in t :clock-resume t))))

; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-agenda-custom-commands
      (quote (("w" "Tasks waiting on something" tags "WAITING/!"
               ((org-use-tag-inheritance nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-todo-ignore-with-date nil)
                (org-agenda-overriding-header "Waiting Tasks")))
              ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE"
               ((org-agenda-todo-ignore-with-date nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-overriding-header "Tasks to Refile")))
              ("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")))
              ("n" "Next" tags-todo "-WAITING-CANCELLED/!NEXT"
               ((org-agenda-overriding-header "Next Tasks")))
              ("p" "Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED-WAITING-SOMEDAY"
               ((org-agenda-skip-function 'bh/skip-non-projects)
                (org-agenda-overriding-header "Projects")))
              ("o" "Other (Non-Project) tasks" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED-WAITING-SOMEDAY"
               ((org-agenda-skip-function 'bh/skip-projects)
                (org-agenda-overriding-header "Other Non-Project Tasks")))
              ("A" "Tasks to be Archived" tags "LEVEL=2-REFILE/DONE|CANCELLED"
               ((org-agenda-overriding-header "Tasks to Archive")))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-todo-ignore-with-date nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-overriding-header "Habits")))
              ("#" "Stuck Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
               ((org-agenda-skip-function 'bh/skip-non-stuck-projects)
                (org-agenda-overriding-header "Stuck Projects")))
              ("c" "Select default clocking task" tags "LEVEL=2-REFILE"
               ((org-agenda-skip-function
                 '(org-agenda-skip-subtree-if 'notregexp "^\\*\\* Organization"))
                (org-agenda-overriding-header "Set default clocking task with C-u C-u I"))))))

(setq org-startup-indented nil)
;; Note taking
(require 'cdlatex)
;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; Time Tracking
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
(setq org-global-properties (quote (
  ("Effort_ALL" . "0:05 0:10 0:15 0:20 0:30 1:00 1:30 2:00 3:00 4:00 5:00"))))

;; CUSTOM CITATION MANAGER
(setq org-citation-root "/Users/welinder/Dropbox/org/library/papers/")
(org-add-link-type "c" 'org-anything-for-citation)
(add-to-list 'auto-mode-alist '("\\.eb$" . org-mode))

(defvar anything-c-source-org-citation-files
  '((name . "Open citation file:")
    (init . (lambda () nil))
    (candidates . (lambda () 
      (list (concat org-citation-root org-citation-current ".eb")
            (concat org-citation-root org-citation-current ".pdf"))))
    (type . file))
  "List files associated with citation.")

(defun org-anything-for-citation (citation-key)
  "Opens a citation file using anything."
  (interactive)
  (setq org-citation-current citation-key)
  (anything '(anything-c-source-org-citation-files)))

;; TODO: should add custom store link