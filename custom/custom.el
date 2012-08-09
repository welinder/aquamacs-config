;;; Main Usability Settings

;; Indention
(setq-default tab-width 4) ; default tab width
(setq-default indent-tabs-mode nil) ; replaces tabs with spaces

;; show paired parenthasis
(show-paren-mode 1)

;; use UTF-8
(prefer-coding-system 'utf-8)

; Lines shouldn't be longer than 76 chars
(setq fill-column 76)

;; remove the beeping, it drives me nuts
(setq ring-bell-function 'ignore)

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; show trailing whitespace in red
;; TODO: make sure this is global (maybe through custom)
(setq show-trailing-whitespace t)
(require 'whitespace)
;(global-whitespace-mode 0) ; TODO: customize this

;; indent whole buffer: http://emacsblog.org/2007/01/17/indent-whole-buffer/
(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; search for word under pointer
(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))
(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))
(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

;; undo-tree
(require 'undo-tree)

;;; auto-complete
(setq auto-complete-dir (concat config-root-dir "plugins/auto-complete-1.3.1/"))
(add-to-list 'load-path auto-complete-dir)
(setq ac-dictionary-directories ())
(add-to-list 'ac-dictionary-directories (concat auto-complete-dir "dict/"))
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; better parentheses pairing
(require 'autopair)
;(autopair-global-mode) ;; enable autopair in all buffers

;; dired settings
(setq dired-dwim-target t) ; allow easier copy to other buffer
;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)
;; enable dired-x
(require 'dired-x)
;; setup wdired
(require 'wdired)
(setq wdired-allow-to-change-permissions 'advanced)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)
;; auto-revert dired buffer to be in sync w file sys
;; http://nflath.com/2009/07/dired/
(defadvice switch-to-buffer-other-window 
  (after auto-refresh-dired (buffer &optional norecord) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))
(defadvice switch-to-buffer 
  (after auto-refresh-dired (buffer &optional norecord) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))
(defadvice display-buffer 
  (after auto-refresh-dired (buffer &optional not-this-window frame)  activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))
(defadvice other-window 
  (after auto-refresh-dired (arg &optional all-frame) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))

;;; EDITING

;; Killing/copying lines when nothing is selected
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(provide 'custom)
