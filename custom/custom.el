;;; Main Usability Settings

;; Indention
(setq-default tab-width 2) ; default tab width
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
(autopair-global-mode) ;; enable autopair in all buffers 

(provide 'custom)
