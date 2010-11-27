;;; init.el --- Where all the magic begins

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Set up helper functions
(setq config-root-dir (file-name-directory
  (or (buffer-file-name) load-file-name)))

;; Add plugins
(add-to-list 'load-path (concat config-root-dir "/plugins"))

;; Add custom configurations
(defconst custom-config-dir (concat config-root-dir "/custom/")
  "Path to directory containing user customized configurations.")
(defun load-config-files (filelist)
  (dolist (file filelist)
    (load (expand-file-name
           (concat custom-config-dir file)))
    (message "Loaded custom config file: %s" file)
  ))

(load-config-files '("custom"
		     "appearance"
		     "keybindings"
		     "yasnippet"
         "ido"
         "python"
         "ecb"
         "multi-term"))

(provide 'init)
