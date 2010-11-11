;;; Main Usability Settings

;; Indention
(setq-default tab-width 2) ; default tab width
(setq-default indent-tabs-mode nil) ; replaces tabs with spaces

;; show paired parenthasis
(show-paren-mode 1)

;; use UTF-8
(prefer-coding-system 'utf-8)

; Lines shouldn't be longer than 79 chars
(setq fill-column 72)

;; remove the beeping, it drives me nuts
(setq ring-bell-function 'ignore)

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

