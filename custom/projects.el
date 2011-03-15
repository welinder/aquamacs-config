;; Project Management

;; unique buffer names
(require 'uniquify)
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;;;-------------------------------------------------------------------------
;;; eproject project management
;;;-------------------------------------------------------------------------
(add-to-list 'load-path (concat config-root-dir "/plugins/eproject"))
(require 'eproject)
(require 'eproject-extras)
;; eproject global bindings
;; See last part of following page to make these act on other projects:
;; https://github.com/jrockway/eproject/wiki/InstallingEproject
(global-set-key (kbd "C-x p k") 'eproject-kill-project-buffers)
(global-set-key (kbd "C-x p v") 'eproject-revisit-project)
(global-set-key (kbd "C-x p b") 'eproject-ibuffer)
(global-set-key (kbd "C-c C-f") 'eproject-find-file)

;;; Modify the generic project
(define-project-type generic () nil
  :relevant-files (".*")
  :irrelevant-files ("^[.]" "^[#]" "\\.\\(png\\|gif\\)")
  :file-name-map (lambda (root) (lambda (root file) file))
  :local-variables (lambda (root) (lambda (root file) nil))
  :config-file ".eproject")

;;;-------------------------------------------------------------------------
;;; ido-mode for navigating projects
;;;-------------------------------------------------------------------------
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(ido-mode 1)
;; set up ignored files
(add-to-list 'ido-ignore-files "\\`\\.DS_Store")

;;;-------------------------------------------------------------------------
;;; anything.el setup
;;;-------------------------------------------------------------------------
(require 'anything-config)
(require 'anything-match-plugin)
;; source: find file in eproject
(defvar anything-c-source-eproject-files
  '((name . "Files in eProject")
    (init . (lambda ()
		  (setq anything-eproject-file-map 
            (mapcar #'eproject--shorten-filename (eproject-list-project-files)))
	  ))
    (candidates . anything-eproject-file-map)
    (type . file))
  "Search for files in this project.")

;; source: find file in ebuffer
(defvar anything-c-source-eproject-buffers
  '((name . "Buffers in this eProject")
    (init . (lambda () (if (buffer-file-name)
      (setq anything-eproject-root-dir (eproject-maybe-turn-on))
      (setq anything-eproject-root-dir 'nil))))
    (candidates . (lambda () (if anything-eproject-root-dir
      (mapcar 'buffer-name  (cdr 
        (assoc anything-eproject-root-dir (eproject--project-buffers)))))))
    (volatile)
    (type . buffer)
    )
  "Search for buffers in this project.")

(defun anything-for-files ()
  "Preconfigured `anything' for opening buffers. Searches for buffers in the current project, then other buffers, also gives option of recentf. Replaces switch-to-buffer."
  (interactive)
  (anything '(anything-c-source-eproject-buffers
              anything-c-source-eproject-files
              )))
(defun anything-for-buffers ()
  "Preconfigured `anything' for opening buffers. Searches for buffers in the current project, then other buffers, also gives option of recentf. Replaces switch-to-buffer."
  (interactive)
  (anything '(anything-c-source-eproject-buffers
	      anything-c-source-buffers+
	      anything-c-source-buffer-not-found
	      anything-c-source-recentf
	      )))

(define-key osx-key-mode-map (kbd "A-t") 'anything-for-files)
;;;-------------------------------------------------------------------------
;;; window setup
;;;-------------------------------------------------------------------------
;; from https://github.com/jrockway/
(require 'window-number)
(defun first-matching-buffer (predicate)
  "Return PREDICATE applied to the first buffer where PREDICATE applied to the buffer yields a non-nil value."
  (loop for buf in (buffer-list)
        when (with-current-buffer buf (funcall predicate buf))
        return (with-current-buffer buf (funcall predicate buf))))

(defun fix-windows ()
  "Setup my window config."
  (interactive)
  (let ((current-project
         (first-matching-buffer (lambda (x) (ignore-errors (eproject-name)))))
        (current-shell
         (or (first-matching-buffer (lambda (x)
                                      (and (or (eq major-mode 'eshell-mode)
                                               (eq major-mode 'term-mode))
                                           x)))
             (eshell))))
    (delete-other-windows)
    (split-window-horizontally)
;    (window-number-select 2)
;    (split-window-vertically)
    (labels 
      ((show (x) 
        (set-window-buffer nil 
          (or x (get-buffer-create "*scratch*")))))
;      (window-number-select 2)
;      (show current-irc-window)
;      (window-number-select 2)
;      (show current-shell)
      (let ((cur))
        (loop for i in '(1 2)
              do
              (window-number-select i)
              (show (first-matching-buffer
                     (lambda (x) (and (equal (ignore-errors (eproject-name))
                                             current-project)
                                      (not (equal cur (buffer-name x)))
                                      x))))
              (setf cur (buffer-name (current-buffer))))))
    (balance-windows)))

(provide 'projects)
