;; CEDET and ECB configuration

;;; CEDET
;; Builing CEDET:
;; `/Applications/Aquamacs.app/Contents/MacOS/Aquamacs -Q -nw -l cedet-build.el -f cedet-build -f save-buffers-kill-terminal`
;; Load CEDET
(load-file (concat config-root-dir "/plugins/cedet-1.0/common/cedet.el"))

;;; ECB
;; Building: just run `ecb-byte-compile`
;; Load ECB
(add-to-list 'load-path (concat config-root-dir "/plugins/ecb-2.40"))
(require 'ecb-autoloads) ; run using ecb-activate
