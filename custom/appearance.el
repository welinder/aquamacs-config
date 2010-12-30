;;; Making the Look and Feel of Aquamacs even better
;;; (some basic settings in init.el for speedy evaluation)

;; fonts
(set-default-font "Inconsolata-12")

;; cursor line
;(global-hl-line-mode 1)
;(set-face-background 'hl-line "#303040")
;(set-cursor-color "#676787")

;; make pretty
(global-font-lock-mode 1)

;; shows current selected region
(setq-default transient-mark-mode t)

;; titlebar = buffer unless filename
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
 
;; line numbers
(global-linum-mode -1) ; use "C-x l" keybinding instead
(setq column-number-mode  t)

;; make side by side buffers function the same as the main window
(setq truncate-partial-width-windows nil)

;; theme
(add-to-list 'load-path (concat config-root-dir "/themes"))
(require 'color-theme-wombat)
(color-theme-wombat) ; select the theme

;; highlight current line
(global-hl-line-mode 1)
; (set-face-background 'hl-line "#330")

;; darkroom-mode
(add-to-list 'load-path (concat config-root-dir "/plugins/darkroom-mode"))
(require 'darkroom-mode)
