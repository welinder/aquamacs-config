;; org-mode

(add-to-list 'load-path (concat config-root-dir "/plugins/org-mode/lisp"))
(add-to-list 'load-path (concat config-root-dir "/plugins/org-mode/contrib/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org-install)
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)


;;; Latex Export
;; remove some defaults are they conflict with amsmath
(setq org-export-latex-default-packages-alist '(
  ("AUTO" "inputenc" t)
  ("T1" "fontenc" t)
;  ("" "fixltx2e" nil)
  ("" "graphicx" t)
  ("" "longtable" nil)
  ("" "float" nil)
  ("" "wrapfig" nil)
;  ("" "soul" t)
;  ("" "textcomp" t)
;  ("" "marvosym" t)
;  ("" "wasysym" t)
;  ("" "latexsym" t)
  ("" "amssymb" t)
  ("" "hyperref" nil)
;  "\\tolerance=1000"
))
;; custom extra packages
(setq org-export-latex-packages-alist '(
  ("" "amsmath" nil)
))


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
  ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAIT(w)"
             "SOMEDAY(S!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)"))))
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAIT" ("WAIT" . t))
              ("SOMEDAY" ("SOMEDAY" . t))
              (done ("WAIT"))
              ("TODO" ("WAITING") ("CANCELLED") ("SOMEDAY"))
              ("STARTED" ("WAITING") ("SOMEDAY"))
              ("DONE" ("WAITING") ("CANCELLED")))))
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
      (quote (("w" "Tasks waiting on something" tags "WAIT/!"
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
              ("p" "Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED-WAIT-SOMEDAY"
               ((org-agenda-skip-function 'bh/skip-non-projects)
                (org-agenda-overriding-header "Projects")))
              ("o" "Other (Non-Project) tasks" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED-WAIT-SOMEDAY"
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

;;; Time Tracking
;; turn on persistent tracking
(setq org-clock-persist 'history)
;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
(setq org-global-properties (quote (
  ("Effort_ALL" . "0:05 0:10 0:15 0:20 0:30 1:00 1:30 2:00 3:00 4:00 5:00"))))
;; Yes it's long... but more is better ;)
(setq org-clock-history-length 28)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task state to NEXT when clocking in
(setq org-clock-in-switch-to-state (quote bh/clock-in-to-started))
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK" "CLOCK")))
;; Save clock data in the CLOCK drawer and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer "CLOCK")
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist (quote history))
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/is-project-p-with-open-subtasks ()
  "Any task with a todo keyword subtask"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t))))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (and
               (member (org-get-todo-state) org-todo-keywords-1)
               (not (member (org-get-todo-state) org-done-keywords)))
          (setq has-subtask t))))
    has-subtask))

(defun bh/clock-in-to-started (kw)
  "Switch task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks and tasks with subtasks"
  (if (and (member (org-get-todo-state) (list "TODO" "NEXT"))
           (not (and (boundp 'org-capture-mode) org-capture-mode))
           (not (bh/is-project-p-with-open-subtasks)))
      "STARTED"))

(defun bh/clock-in ()
  (interactive)
  (setq bh/keep-clock-running t)
  (org-agenda nil "c"))

(defun bh/clock-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out)))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running (not org-clock-clocking-in) (marker-buffer org-clock-default-task))
    (bh/clock-in-default-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)




;;; CUSTOM CITATION MANAGER
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