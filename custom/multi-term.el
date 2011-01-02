;; Setup for multi-term.el
;; http://www.emacswiki.org/emacs/MultiTerm

(require 'multi-term)
(setq multi-term-program "/bin/bash") ; the colors in zsh doesn't work well
;(setq multi-term-program "~/bin/eshell")
;(setenv "ESHELL" (expand-file-name "~/bin/eshell"))

;; set colors to match wombat
(custom-set-variables
 '(term-default-bg-color "#242424")        ;; background color (black)
 '(term-default-fg-color "#f6f3e8"))       ;; foreground color (yellow)

;; turn off line highlighting in term mode
(add-hook 'term-mode-hook 
	  (lambda()
      (set (make-local-variable 'global-hl-line-mode) nil)
	    (message "%s" "This is in term mode and hook enabled.")
      ))

