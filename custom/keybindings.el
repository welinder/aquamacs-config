;;; Custom Key Bindings

;; BUFFERS
;; bs.el --- menu for selecting and displaying buffers
(require 'bs)
; show list of buffers
(global-set-key (kbd "<C-tab>") 'bs-show)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
; cycle buffers
(global-set-key (kbd "<M-s-right>") 'bs-cycle-next)
(global-set-key (kbd "<M-s-left>") 'bs-cycle-previous)
; kill buffer the native way with COMMAND-w
(global-set-key (kbd "s-w") 'kill-buffer)
; switch to latest buffer (fast switching)
(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer)))
(global-set-key (kbd "<M-tab>") 'switch-to-previous-buffer)

;; WINDOW NAVIGATION
; cycle through windows
(global-set-key (kbd "<C-s-tab>") 'other-window)

; emacsd-tile.el -- tiling windows for emacs
; stolen from http://monkey.org/~marius/emacs-as-a-tiling-window-manager.html
(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)))))

(global-set-key (kbd "C-M-J") (lambda () (interactive) (swap-with 'down)))
(global-set-key (kbd "C-M-K") (lambda () (interactive) (swap-with 'up)))
(global-set-key (kbd "C-M-H") (lambda () (interactive) (swap-with 'left)))
(global-set-key (kbd "C-M-L") (lambda () (interactive) (swap-with 'right)))

(global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))

(global-set-key (kbd "<A-down>") 'windmove-down)
(global-set-key (kbd "<A-up>") 'windmove-up)
(global-set-key (kbd "<A-left>") 'windmove-left)
(global-set-key (kbd "<A-right>") 'windmove-right)

;;; Enable fullscreen mode with M-RET
;;; http://www.stratospark.com/blog/2010/fullscreen_emacs_on_osx.html
(global-set-key (kbd "M-RET") 'aquamacs-toggle-full-frame)

;; Smex makes M-x commands sooo much easier to access:
;; https://github.com/nonsequitur/smex/
(require 'smex)
(smex-initialize)
;; set up new M-x key bindings 
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex) ; more efficient
;(global-set-key "\C-x\C-M" 'smex-major-mode-commands) ;buggy - replaces previous cmd. ..?
;; old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;; EFFECTIVE EMACS
;;; http://web.archive.org/web/20060903005117/http://www.cabochon.com/~stevey/blog-rants/effective-emacs.html
;; kill word
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; ALT key is really hard to get at, so replace it with command key
(define-key osx-key-mode-map (kbd "A-f") 'forward-word)
(define-key osx-key-mode-map (kbd "A-b") 'backward-word)

;; custom bindings
(global-set-key (kbd "C-x l") 'linum-mode)
(global-set-key (kbd "C-/") 'comment-region)
(global-set-key (kbd "C-?") 'uncomment-region) ; C-S-/
(global-set-key (kbd "C-/") 'comment-region)

(global-set-key (kbd "<M-down>") 'forward-paragraph)
(global-set-key (kbd "<M-up>") 'backward-paragraph)
(global-set-key (kbd "<M-left>") 'backward-word)
(global-set-key (kbd "<M-right>") 'forward-word)

(global-set-key (kbd "C-c s") 'query-replace)
(global-set-key (kbd "C-c r") 'query-replace-regexp)

(global-set-key (kbd "C-x m") 'multi-term-next)

(global-set-key (kbd "C-x m") 'multi-term-next)
(define-key osx-key-mode-map (kbd "A-u") 'ido-switch-buffer)
(define-key osx-key-mode-map (kbd "A-i") 'ibuffer)
