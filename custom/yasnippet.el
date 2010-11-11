
;; The root dir for all YASnippet stuff
(setq yasnippet-dir (concat config-root-dir "/plugins/yasnippet-0.6.1c"))
;; Load and initialize
(add-to-list 'load-path yasnippet-dir)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat yasnippet-dir "/snippets"))
