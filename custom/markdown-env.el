;; Markdown Environment
;; use markdown-mode: http://jblevins.org/projects/markdown-mode/
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

