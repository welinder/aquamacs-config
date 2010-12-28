;; Project Management

(add-to-list 'load-path (concat config-root-dir "/plugins/eproject"))
(require 'eproject)
(require 'eproject-extras)
(require 'anything-config)
(require 'anything-match-plugin)

(defvar anything-c-source-eproject-files
  '((name . "Files in eProject")
    (init . (lambda () (if (buffer-file-name)
		  (setq anything-eproject-root-dir (eproject-maybe-turn-on))
			(setq anything-eproject-root-dir 'nil)
	  )))
    (candidates . (lambda () (if anything-eproject-root-dir
      (eproject-list-project-files anything-eproject-root-dir))))
      (type . file)
  )
  "Search for files in the current eProject.")

(defvar anything-c-source-eproject-buffers
  '((name . "Buffers in this eProject")
    (init . (lambda () (if (buffer-file-name)
      (setq anything-eproject-root-dir (eproject-maybe-turn-on))
      (setq anything-eproject-root-dir 'nil))))
    (candidates . (lambda () (if anything-eproject-root-dir
      (mapcar 'buffer-name  (cdr 
        (assoc anything-eproject-root-dir (eproject—project-buffers)))))))
    (volatile)
    (type . buffer)
    )
  "Search for buffers in this project.")

(defun anything-for-buffers ()
  "Preconfigured `anything' for opening buffers. Searches for buffers in the current project, then other buffers, also gives option of recentf. Replaces switch-to-buffer."
  (interactive)
  (anything '(anything-c-source-eproject-buffers
	      anything-c-source-buffers+
	      anything-c-source-buffer-not-found
	      anything-c-source-recentf
	      )))

(defun anything-for-files ()
  "Preconfigured `anything' for opening buffers. Searches for buffers in the current project, then other buffers, also gives option of recentf. Replaces switch-to-buffer."
  (interactive)
  (anything '(anything-c-source-eproject-files
              anything-c-source-recentf)))
