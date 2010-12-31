;; ---------- .emacs customization file ----------------------------------------

;; This file should load with every stock Emacs 22 and higher, while custom
;; extensions only present on my machine are loaded in extensions.el.

;; PW: check out screenshots here: http://pythonic.pocoo.org/2008/2/17/there-are-many-things-you-can-say-about-emacs

;; last modified: 2008-06-01 09:28 by gbr

;; set up load path
(setq load-path `("/home/gbr/.emacs.d/emacs-goodies-el"
                  "/home/gbr/.emacs.d/predictive"
                  "/home/gbr/.emacs.d/talcum"
                  "/home/gbr/.emacs.d"
                  ,@load-path))

;; don't show so many messages on startup
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; load my extensions if they are present
(load "extensions.el" t)

;; Enable the Emacs server
(server-start)


;; ---------- Some influential variables ---------------------------------------

;; set background color
(set-background-color "gray97")

;; scroll one line at a time
(setq scroll-step 1)

;; make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; window frame title
(setq frame-title-format "emacs [%b %*%+ %f]")
(setq icon-title-format "emacs [%b]")

;; no bells please
(setq ring-bell-function (lambda nil nil))

;; make all backups in a single directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups"))))

;; mouse scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

;; enable otherwise disabled commands
(put 'set-goal-column  'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)

;; remove trailing whitespaces before saving (now local)
;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; update copyright headers before saving
(add-hook 'before-save-hook 'copyright-update)
;; update timestamp ("last modified") before saving
(setq time-stamp-pattern "10/[Ll]ast modified: %:y-%02m-%02d %02H:%02M by %u$")
(add-hook 'before-save-hook 'time-stamp)

;; fetch semantic tags after saving (now local)
;(add-hook 'after-save-hook 'semantic-fetch-tags)
;; make file executable if it's a script
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; hippie-expand functions
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        ;try-expand-list
        ;try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;; ---------- Additional packages to load --------------------------------------

;; nice config file modes
(require 'generic-x)

;; electric bindings for help mode
(require 'ehelp)

;; ---------- Keybindings ------------------------------------------------------

;; find file at point
(ffap-bindings)

;; find files with ":linenum"
(global-set-key (kbd "C-x C-f") 'find-file-with-linenum)

;; display same buffer in other window too
(global-set-key (kbd "C-x C-o") 'display-same-buffer-other-window)

;; indent automatically
(global-set-key (kbd "RET") 'newline-and-indent)

;; file cache
(define-key minibuffer-local-map (kbd "C-f") 'file-cache-minibuffer-complete)

;; useful mouse behavior
(global-set-key (kbd "<s-mouse-1>") 'mouse-delete-other-windows)
(global-set-key (kbd "<s-mouse-3>") 'mouse-delete-window)

;; get a buffer menu with the right mouse button.
(global-set-key (kbd "<mouse-3>") 'mouse-buffer-menu)

;; windmove: easily move between windows
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>")  'windmove-left)

;; custom margin keys (useful for Python indentation)
(global-set-key (kbd "C-M-+") 'increase-left-margin)
(global-set-key (kbd "C-M--") 'decrease-left-margin)

;; compile/make
(global-set-key (kbd "<f5>")   'recompile)
(global-set-key (kbd "S-<f5>") 'compile)

;; fullscreen editing
(global-set-key (kbd "<f11>") 'fullscreen)

;; C-k is kill-whole-line
(global-set-key (kbd "C-k") 'kill-whole-line)

;; shortcuts for killing buffers
(global-set-key (kbd "C-x k")     'kill-this-buffer)
(global-set-key (kbd "C-x K")     'kill-other-buffer)
(global-set-key (kbd "C-x C-k")   'kill-buffer-and-window)
(global-set-key (kbd "C-x C-M-k") 'kill-other-buffer-and-window)

;; F6 stores a position in a file, F7 brings you back to this position
(global-set-key (kbd "<f6>") '(lambda () (interactive) (point-to-register ?1)))
(global-set-key (kbd "<f7>") '(lambda () (interactive) (register-to-point ?1)))

;; really useful feature to avoid over-long lines in source code
(global-set-key (kbd "<f9>") 'highlight-beyond-fill-column)

(global-set-key (kbd "<f8>") 'font-lock-fontify-buffer)

;; moving between compilation errors
;(global-set-key (kbd "<f2>") 'previous-error)
;(global-set-key (kbd "<f3>") 'next-error)

;; Alt-space expands
;(global-set-key (kbd "M-SPC") 'dabbrev-expand)
(global-set-key (kbd "M-SPC") 'hippie-expand)

;; make commenting easy ;)
(global-set-key (kbd "M-#") 'comment-region)
(global-set-key (kbd "C-#") 'comment-region)
;; toggle line numer display
(global-set-key (kbd "C-c n") 'global-linum-mode)

;; global shortcut for occur-mode
(global-set-key (kbd "C-c o") 'occur)

;; scroll without moving cursor
(global-set-key (kbd "C-M-<up>") '(lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "C-M-<down>") '(lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-<up>") '(lambda () (interactive) (scroll-down 5)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (scroll-up 5)))

;; like Vim's '*' binding
(global-set-key (kbd "C-+") 'search-for-this-word)
(global-set-key (kbd "C-x *") 'search-for-this-word)
(global-set-key (kbd "C-*") 'isearch-lazy-highlight-cleanup)

;; fixup-whitespace puts the "right" amount of whitespace at the point
(global-set-key (kbd "S-SPC") 'fixup-whitespace)
;; M-del should delete forward
(global-set-key (kbd "M-<delete>") 'kill-word)

;; M-/ runs shell command with region as stdin
(global-set-key (kbd "M-/") 'shell-command-on-region)
;; M-& runs shell command with region as stdin and replaces it with stdout
(global-set-key (kbd "M-&") (lambda () (interactive)
                              (setq current-prefix-arg (list 4))
                              (call-interactively 'shell-command-on-region)))

;; repeat simple and complex commands
(global-set-key (kbd "C-.") 'repeat)


;; ---------- Modes ------------------------------------------------------------

;; auto-fill in text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;; nice xterm mouse handling
(xterm-mouse-mode t)

;; abbrev file for abbrev-mode
(read-abbrev-file "~/.abbrevs")

;; ignore alltt environments in spell checking
(eval-after-load "ispell"
  '(let ((list (cadr ispell-tex-skip-alists)))
     (add-to-list 'list '("alltt" . "\\\\end[ \t\n]*{[ \t\n]*alltt[ \t\n]*}"))
     (setcdr ispell-tex-skip-alists (list list))))

;; highlight XXX style code tags in source files
(font-lock-add-keywords 'python-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))

(font-lock-add-keywords 'c-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))

(font-lock-add-keywords 'latex-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))


;; ---------- Mode-specific keybindings ----------------------------------------

;; C-k is "kill match" in isearch
(define-key isearch-mode-map (kbd "C-k") 'kill-isearch-match)
;; C-o is occur in isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; next/previous match in occur
(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)

;; close HTML tags with C-t in sgml mode
(add-hook 'sgml-mode-hook
	  (lambda () (local-set-key (kbd "C-t") 'sgml-close-tag)))

;; more powerful tab-completion in minibuffer
(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (define-key minibuffer-local-map "\t" 'comint-dynamic-complete)))


;; ---------- C mode specifics -------------------------------------------------

(c-add-style
 "python-new"
 '((indent-tabs-mode . nil)
   (fill-column      . 78)
   (c-basic-offset   . 4)
   (c-offsets-alist  . ((substatement-open . 0)
                        (inextern-lang . 0)
                        (arglist-intro . +)
                        (knr-argdecl-intro . +)))
   (c-hanging-braces-alist . ((brace-list-open)
                              (brace-list-intro)
                              (brace-list-close)
                              (brace-entry-open)
                              (substatement-open after)
                              (block-close . c-snug-do-while)))
   (c-block-comment-prefix . "* ")))

(c-add-style
 "javascript"
 '((indent-tabs-mode . nil)
   (fill-column      . 78)
   (c-basic-offset   . 2)
   (c-offsets-alist  . ((substatement-open . 0)
                        (inextern-lang . 0)
                        (arglist-intro . +)
                        (knr-argdecl-intro . +)))
   (c-hanging-braces-alist . ((brace-list-open)
                              (brace-list-intro)
                              (brace-list-close)
                              (brace-entry-open)
                              (substatement-open after)
                              (block-close . c-snug-do-while)))
   (c-block-comment-prefix . "")))

(add-to-list 'c-default-style '(c-mode . "python-new"))
(add-to-list 'c-default-style '(ecmascript-mode . "javascript"))

(defun c-select-style ()
  "Select the C style to use from buffer indentation."
  (save-excursion
    (if (re-search-forward "^\t" 3000 t)
        (c-set-style "python")
      (c-set-style "python-new"))))

(add-hook 'c-mode-hook 'c-select-style)


;; ---------- Python mode specifics --------------------------------------------

(defun my-python-goodies ()
  (show-paren-mode 1)
  (reveal-mode 1)
  (set-variable 'show-trailing-whitespace 1)
  (local-set-key (kbd "C-c a") 'py-beginning-of-def-or-class)
  (local-set-key (kbd "M-<right>") 'py-forward-into-nomenclature)
  (local-set-key (kbd "M-<left>") 'py-backward-into-nomenclature)
  (local-set-key (kbd "M-DEL") 'py-backward-kill-nomenclature)

  ;; add some local hooks
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (add-hook 'after-save-hook 'semantic-fetch-tags nil t)

  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "python " buffer-file-name)))
)

(add-hook 'python-mode-hook 'my-python-goodies)


;; ---------- Haskell mode specifics -------------------------------------------

;; support flymake in Haskell mode
(require 'flymake)

(defun flymake-Haskell-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-Haskell-cmdline))

(defun flymake-get-Haskell-cmdline (source base-dir)
  (list "flycheck_haskell.pl"
        (list source base-dir)))

(push '(".+\\.hs$" flymake-Haskell-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)
(push '(".+\\.lhs$" flymake-Haskell-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)
(push
 '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
   1 2 3 4) flymake-err-line-patterns)


;; ---------- Custom interactive functions -------------------------------------

(defun display-same-buffer-other-window ()
  "Display the current buffer in the other window too."
  (interactive)
  (let* ((buffer (current-buffer)))
    (other-window 1)
    (switch-to-buffer buffer)))

(defun kill-other-buffer-and-window ()
  "Kill other window's buffer and the window."
  (interactive "")
  (other-window 1)
  (kill-buffer-and-window))

(defun fullscreen ()
  "Toggle fullscreen editing."
  (interactive)
  (menu-bar-mode)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun kill-other-buffer ()
  "Kill other window's buffer."
  (interactive)
  (other-window 1)
  (kill-buffer nil)
  (other-window 1))

(defun find-file-with-linenum ()
  "Find file and go to line number specifed with :num."
  (interactive)
  (let* ((fname (ffap-prompter))
         (cpos (string-match ":" fname))
         (fpos (or cpos (length fname))))
    (find-file-at-point (substring fname 0 fpos))
    (when cpos (goto-line (string-to-number (substring fname (1+ cpos)))))))

(defun kill-isearch-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point)))

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defvar current-this-regex "")
(defun search-for-this-word ()
  "Emulate Vim's `*' binding."
  (interactive)
  (let ((tag (find-tag-default)))
    (if tag (setq new-this-regex
                  (concat "\\<" (regexp-quote tag) "\\>"))
      (error "point not over tag")))
  (unless (string-equal new-this-regex current-this-regex)
    (font-lock-remove-keywords
     nil (list (list current-this-regex 0 'lazy-highlight-face t)))
    (font-lock-add-keywords
     nil (list (list new-this-regex 0 'lazy-highlight-face t)))
    (setq current-this-regex new-this-regex)
    (font-lock-fontify-buffer)
    (message (concat "Searching for " (substring new-this-regex 2 -2))))
  (unless (search-forward-regexp current-this-regex nil t
                                 (if (looking-at "\\<") 2 1))
    (beginning-of-buffer)
    (message "search hit BOTTOM, continuing at TOP")
    (search-forward-regexp current-this-regex))
  (while (not (looking-at current-this-regex))
    (backward-char 1))
)

(require 'grep)
(defun grep (regexp &optional files)
  "Always grep from the current directory."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
            (files (grep-read-files regexp)))
       (list regexp files))))
  (rgrep regexp files default-directory))

(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

(defun list-overlays-at (&optional pos)
  "Describe overlays at POS or point."
  (interactive)
  (setq pos (or pos (point)))
  (let ((overlays (overlays-at pos))
        (obuf (current-buffer))
        (buf (get-buffer-create "*Overlays*"))
        (props '(priority window category face mouse-face display
                          help-echo modification-hooks insert-in-front-hooks
                          insert-behind-hooks invisible intangible
                          isearch-open-invisible isearch-open-invisible-temporary
                          before-string after-string evaporate local-map keymap
                          field))
        start end text)
    (if (not overlays)
        (message "None.")
      (set-buffer buf)
      (erase-buffer)
      (dolist (o overlays)
        (setq start (overlay-start o)
              end (overlay-end o)
              text (with-current-buffer obuf
                     (buffer-substring start end)))
        (when (> (- end start) 13)
          (setq text (concat (substring text 1 10) "...")))
        (insert (format "From %d to %d: \"%s\":\n" start end text))
        (dolist (p props)
          (when (overlay-get o p)
            (insert (format " %15S: %S\n" p (overlay-get o p))))))
      (pop-to-buffer buf))))


;; ---------- Stuff managed by Emacs -------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-babel-hyphen "")
 '(LaTeX-fill-break-at-separators (quote (\\\( \\\) \\\[ \\\])))
 '(LaTeX-item-indent -2)
 '(LaTeX-math-abbrev-prefix "`")
 '(LaTeX-menu-max-items 40)
 '(LaTeX-mode-hook (quote (preview-mode-setup talcum-mode LaTeX-install-toolbar turn-on-reftex LaTeX-math-mode auto-fill-mode)))
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "alltt" "listing")))
 '(LaTeX-verbatim-regexp "verbatim\\*?\\|alltt\\|listing")
 '(TeX-auto-local ".auto/")
 '(TeX-bar-LaTeX-buttons (quote (open-file save-buffer cut copy paste undo [separator nil] latex next-error view file bibtex clean latex-symbols-experimental nil)))
 '(TeX-close-quote "\"'")
 '(TeX-electric-escape nil)
 '(TeX-insert-braces nil)
 '(TeX-master t)
 '(TeX-newline-function (quote newline-and-indent))
 '(TeX-open-quote "\"`")
 '(TeX-parse-self t)
 '(TeX-style-local ".style/")
 '(ansi-color-for-comint-mode t)
 '(bm-repository-file "/home/gbr/.emacs.d/bm-repository")
 '(browse-url-browser-function (quote browse-url-mozilla))
 '(browse-url-mozilla-program "firefox")
 '(c-electric-pound-behavior (quote (alignleft)))
 '(column-number-mode t)
 '(comint-input-autoexpand (quote history))
 '(comint-move-point-for-output (quote this))
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input (quote this))
 '(comment-line-break-function (quote comment-indent-new-line) t)
 '(comment-style (quote box-multi))
 '(completion-auto-show-menu t)
 '(copyright-names-regexp "Georg Brandl")
 '(css-electric-brace-behavior t)
 '(css-electric-semi-behavior t)
 '(cua-enable-cua-keys (quote shift))
 '(cua-enable-cursor-indications t)
 '(cua-mode t nil (cua-base))
 '(cua-prefix-override-inhibit-delay 0.3)
 '(cursor-in-non-selected-windows t)
 '(desktop-path (quote ("~/.emacs.d")))
 '(dired-recursive-deletes (quote top))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(dnd-open-file-other-window t)
 '(ecb-directories-menu-user-extension-function (quote ignore))
 '(ecb-display-image-icons-for-semantic-tags t)
 '(ecb-fix-window-size (quote width))
 '(ecb-grep-find-function (quote if))
 '(ecb-highlight-tag-with-point (quote highlight-scroll))
 '(ecb-history-item-name (quote buffer-name))
 '(ecb-history-menu-user-extension-function (quote ignore))
 '(ecb-kill-buffer-clears-history (quote auto))
 '(ecb-layout-name "left7")
 '(ecb-layout-window-sizes (quote (("left14" (0.14977973568281938 . 0.7258064516129032) (0.14977973568281938 . 0.25806451612903225)) ("left7" (0.24858757062146894 . 0.48333333333333334) (0.24858757062146894 . 0.18333333333333332) (0.24858757062146894 . 0.31666666666666665)) ("left8" (0.23076923076923078 . 0.2830188679245283) (0.23076923076923078 . 0.22641509433962265) (0.23076923076923078 . 0.3018867924528302) (0.23076923076923078 . 0.16981132075471697)))))
 '(ecb-method-non-semantic-face (quote ecb-default-general-face))
 '(ecb-methods-menu-user-extension-function (quote ignore))
 '(ecb-minor-mode-text "")
 '(ecb-non-semantic-exclude-modes (quote (sh-mode fundamental-mode text-mode LaTeX-mode)))
 '(ecb-options-version "2.32")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
 '(ecb-process-non-semantic-files t)
 '(ecb-source-file-regexps (quote ((".*" ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(pyc\\|elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)") ("^\\.\\(emacs\\|gnus\\)$")))))
 '(ecb-source-path (quote ("~/devel" "/")))
 '(ecb-sources-menu-user-extension-function (quote ignore))
 '(ecb-sources-sort-method (quote extension))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-expand-symbol-before t)
 '(ecb-tree-image-icons-directories (quote ("/usr/share/emacs/site-lisp/ecb/ecb-images/default/height-17" (ecb-directories-buffer-name . "/usr/share/emacs/site-lisp/ecb/ecb-images/directories/height-17") (ecb-sources-buffer-name . "/usr/share/emacs/site-lisp/ecb/ecb-images/sources/height-14_to_21") (ecb-methods-buffer-name . "/usr/share/emacs/site-lisp/ecb/ecb-images/methods/height-14_to_21"))))
 '(ecb-use-speedbar-instead-native-tree-buffer nil)
 '(ecb-version-check t)
 '(ecb-windows-width 0.25)
 '(file-cache-buffer "*File Cache*")
 '(file-cache-filter-regexps (quote ("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" ",v$" "\\.output$" "\\.$" "#$" "\\.class$" "\\.pyc$" "\\.svn/.*$")))
 '(file-cache-find-command-posix-flag t)
 '(fill-column 80)
 '(filladapt-mode-line-string " fa")
 '(flyspell-default-dictionary "german")
 '(flyspell-issue-message-flag t)
 '(flyspell-use-meta-tab nil)
 '(folding-allow-overlays t)
 '(font-latex-quotes (quote auto))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(gc-cons-threshold 4000000)
 '(gdb-many-windows t)
 '(glasses-original-separator "‿")
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "‗")
 '(glasses-uncapitalize-p t)
 '(glasses-uncapitalize-regexp "[a-z]")
 '(global-font-lock-mode t nil (font-core))
 '(global-hl-line-mode t)
 '(grep-files-aliases (quote (("asm" . "*.[sS]") ("c" . "*.c") ("cc" . "*.cc") ("ch" . "*.[ch]") ("el" . "*.el") ("h" . "*.h") ("l" . "[Cc]hange[Ll]og*") ("m" . "[Mm]akefile*") ("tex" . "*.tex") ("texi" . "*.texi") (rst . "*.rst") (py . "*.py"))))
 '(grep-find-command "find . -name .svn -prune -o -type f -print0 | xargs -0 -e grep -nHE -e ")
 '(grep-find-ignored-directories (quote ("CVS" ".svn" ".git" ".hg" ".bzr" "_darcs")))
 '(grep-highlight-matches t)
 '(gud-tooltip-echo-area nil)
 '(gud-tooltip-mode t)
 '(haskell-font-lock-symbols nil)
 '(haskell-indent-offset 2)
 '(haskell-mode-hook (quote (turn-on-haskell-indent imenu-add-menubar-index turn-on-haskell-doc-mode)) t)
 '(haskell-program-name "ghci")
 '(history-delete-duplicates t)
 '(icomplete-mode nil)
 '(ido-decorations (quote ("[" "]" ", " ", ..." "[" "]" " [No match]" " [Matched]")))
 '(ido-enable-dot-prefix t)
 '(ido-enable-flex-matching t)
 '(ido-enable-prefix nil)
 '(ido-enabled (quote buffer) nil (ido))
 '(ido-everywhere t)
 '(ido-ignore-buffers (quote ("\\` " "\\*Pymacs\\*" "\\*Help\\*" "\\*Completions\\*" "\\*Messages\\*" "\\*Ido Completions\\*")))
 '(ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`\\.svn/")))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.svn/")))
 '(ido-mode (quote both) nil (ido))
 '(igrep-options (quote -i) t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inferior-haskell-wait-and-jump t)
 '(inhibit-startup-echo-area-message nil)
 '(initial-buffer-choice t)
 '(initial-scratch-message ";; Scratch buffer
")
 '(isearch-allow-scroll t)
 '(isearch-resume-in-command-history nil)
 '(ispell-alternate-dictionary "/usr/lib/ispell/german.hash")
 '(ispell-complete-word-dict "/usr/lib/ispell/german.hash")
 '(ispell-extra-args (quote ("-W2")))
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-local-dictionary-alist nil)
 '(ispell-program-name "aspell")
 '(iswitchb-buffer-ignore (quote ("^ " "\\*Pymacs\\*" "\\*Completions\\*" "\\*Help\\*" "\\*Messages\\*" "\\*rope-")))
 '(iswitchb-use-virtual-buffers t nil (recentf))
 '(jit-lock-context-time 0.2)
 '(jit-lock-contextually t)
 '(jit-lock-defer-time nil)
 '(kill-whole-line t)
 '(lazy-highlight-cleanup nil)
 '(list-directory-brief-switches "-1")
 '(make-backup-file-name-function nil)
 '(make-backup-files nil)
 '(make-cursor-line-fully-visible t t)
 '(margin-column 80)
 '(minibuffer-electric-default-mode t)
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mumamo-chunk-coloring (quote submode-colored))
 '(mumamo-set-major-mode-delay 0.3)
 '(next-screen-context-lines 5)
 '(normal-erase-is-backspace t)
 '(nxhtml-skip-welcome t)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files (quote ("/home/gbr/.org/python.org")))
 '(org-cycle-global-at-bob t)
 '(org-ellipsis nil)
 '(org-fontify-done-headline t)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-highlight-latex-fragments-and-specials t)
 '(org-replace-disputed-keys t)
 '(org-special-ctrl-a/e t)
 '(outline-blank-line t t)
 '(overflow-newline-into-fringe t)
 '(paste-kill-url t)
 '(paste-show-in-browser nil)
 '(pop-up-windows nil)
 '(preview-auto-cache-preamble t)
 '(preview-default-document-pt 11)
 '(preview-transparent-color t)
 '(py-honor-comment-indentation (quote other))
 '(py-imenu-show-method-args-p t)
 '(py-shell-switch-buffers-on-execute nil)
 '(python-honour-comment-indentation nil)
 '(python-use-skeletons nil)
 '(read-quoted-char-radix 10)
 '(recentf-max-saved-items 100)
 '(recentf-menu-open-all-flag t)
 '(recentf-mode t)
 '(recentf-save-file "~/.recentf")
 '(reftex-enable-partial-scans t)
 '(reftex-include-file-commands (quote ("include" "input" "includedoc")))
 '(reftex-plug-into-AUCTeX t)
 '(reftex-save-parse-info t)
 '(reftex-use-multiple-selection-buffers t)
 '(reftex-vref-is-default t)
 '(require-final-newline (quote ask))
 '(rng-nxml-auto-validate-flag nil)
 '(ropemacs-completing-read-function (quote ido-completing-read))
 '(rst-definition-face (quote font-lock-function-name-face))
 '(rst-directive-face (quote font-lock-builtin-face))
 '(rst-level-face-base-color "grey")
 '(rst-level-face-base-light 85)
 '(rst-level-face-step-light -7)
 '(rst-mode-hook nil)
 '(rst-mode-lazy nil)
 '(save-place t nil (saveplace))
 '(screen-lines-minor-mode-string " \\/")
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 0)
 '(search-exit-option t)
 '(search-upper-case nil)
 '(semantic-idle-scheduler-idle-time 200)
 '(semantic-stickyfunc-indent-string #("   " 0 3 (auto-composed t)))
 '(semantic-tag-folding-allow-folding-of (quote ((type) (function) (variable) (include) (comment) (package))))
 '(semantic-tag-folding-fringe-image-style (quote triangles))
 '(semantic-tag-folding-highlight-tags-shown-by-reveal-mode t)
 '(semantic-tag-folding-show-tooltips t)
 '(semanticdb-default-file-name ".semantic.cache")
 '(semanticdb-default-save-directory "~/.emacs.d/.semantic")
 '(show-paren-mode t)
 '(show-ws-style (quote color))
 '(size-indication-mode t)
 '(snippet-bound-face (quote font-latex-italic-face))
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images t)
 '(split-window-preferred-function (quote split-window-preferred-horizontally))
 '(tab-width 8)
 '(tabbar-background-color "gray90")
 '(tabbar-cycle-scope (quote tabs))
 '(tabbar-separator (quote (1)))
 '(table-time-before-update 0)
 '(talcum-desired-features (quote (talcum-render talcum-newcmd)))
 '(talcum-use-prod-flag nil)
 '(talcum-verbosity 7)
 '(tex-close-quote "\"'")
 '(tex-open-quote "\"`")
 '(tool-bar-mode nil)
 '(tooltip-delay 1)
 '(tooltip-short-delay 0.5)
 '(tramp-debug-buffer nil)
 '(tramp-verbose 5)
 '(trex-unicode-mappings (quote (("forall" . 8704) ("complement" . 8705) ("partial" . 8706) ("exists" . 8707) ("emptyset" . 8709) ("nabla" . 8711) ("in" . 8712) ("notin" . 8713) ("ni" . 8715) ("qedhere" . 8718) ("prod" . 8719) ("coprod" . 8720) ("sum" . 8721) ("mp" . 8723) ("setminus" . 8726) ("circ" . 8728) ("cdot" . 8729) ("sqrt" . 8730) ("infty" . 8734) ("land" . 8743) ("wedge" . 8743) ("lor" . 8744) ("vee" . 8744) ("cap" . 8745) ("cup" . 8746) ("int" . 8747) ("iint" . 8748) ("iiiint" . 8749) ("neq" . 8800) ("ne" . 8800) ("leq" . 8804) ("le" . 8804) ("geq" . 8805) ("ge" . 8805) ("prec" . 8826) ("succ" . 8827) ("subset" . 8834) ("supset" . 8835) ("subseteq" . 8838) ("supseteq" . 8839) ("subsetneq" . 8842) ("supsetneq" . 8843) ("unlhd" . 8884) ("lhd" . 8882) ("unrhd" . 8885) ("rhd" . 8883) ("implies" . 10233) ("iff" . 10234) ("mapsto" . 10236) ("to" . 10230) ("longleftarrow" . 10229) ("longrightarrow" . 10230) ("longleftrightarrow" . 10231) ("Longleftarrow" . 10232) ("Longrightarrow" . 10233) ("leftarrow" . 8592) ("uparrow" . 8593) ("rightarrow" . 8594) ("downarrow" . 8595) ("leftrightarrow" . 8596) ("updownarrow" . 8597) ("dots" . 8230) ("ldots" . 8230) ("textperthousand" . 8240) ("bigodot" . 10752) ("bigoplus" . 10753) ("bigotimes" . 10754) ("lneq" . 10887) ("gneq" . 10888) ("wp" . 8472) ("ell" . 8467) ("Im" . 8465) ("Re" . 8476) ("Finv" . 8498) ("Game" . 8513) ("aleph" . 8501) ("beth" . 8502) ("gimel" . 8503) ("daleth" . 8504) ("alpha" . 945) ("beta" . 946) ("gamma" . 947) ("delta" . 948) ("epsilon" . 1013) ("varepsilon" . 949) ("zeta" . 950) ("eta" . 951) ("theta" . 952) ("vartheta" . 977) ("iota" . 953) ("kappa" . 954) ("varkappa" . 1008) ("lambda" . 955) ("mu" . 956) ("nu" . 957) ("xi" . 958) ("pi" . 960) ("varpi" . 982) ("rho" . 961) ("varrho" . 1009) ("sigma" . 963) ("varsigma" . 962) ("tau" . 964) ("upsilon" . 965) ("varphi" . 966) ("phi" . 981) ("chi" . 967) ("psi" . 968) ("omega" . 969) ("digamma" . 989) ("Gamma" . 915) ("Delta" . 916) ("Theta" . 920) ("Lambda" . 923) ("Xi" . 926) ("Pi" . 928) ("Sigma" . 931) ("Upsilon" . 933) ("Phi" . 934) ("Psi" . 936) ("Omega" . 937) ("N" . 8469) ("R" . 8477) ("Q" . 8474) ("C" . 8450) ("Z" . 8484) ("pm" . 177))))
 '(truncate-partial-width-windows nil)
 '(undo-limit 200000)
 '(undo-strong-limit 300000)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(visible-cursor t)
 '(wdired-allow-to-change-permissions t)
 '(x-select-enable-clipboard t)
 '(xhtml-multi-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "gray97" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 95 :width normal :family "microsoft-Consolas"))))
 '(company-pseudo-tooltip-selection-face ((t (:inherit company-pseudo-tooltip-face :background "#ff6600"))))
 '(custom-button ((((type x w32 mac) (class color)) (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button) :height 90 :family "microsoft-tahoma"))))
 '(custom-button-face ((((type x w32 mac) (class color)) (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button) :height 1.1 :family "microsoft-tahoma"))) t)
 '(custom-button-mouse ((((type x w32 mac) (class color)) (:inherit custom-button :background "grey90" :foreground "black" :box (:line-width 2 :style released-button)))))
 '(custom-button-pressed ((((type x w32 mac) (class color)) (:inherit custom-button :background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))
 '(custom-button-pressed-face ((((type x w32 mac) (class color)) (:inherit custom-button-face :box (:line-width 2 :style pressed-button)))) t)
 '(custom-changed-face ((((class color)) (:inherit custom-documentation-face :background "blue" :foreground "white"))) t)
 '(custom-comment ((((class grayscale color) (background light)) (:inherit custom-documentation :background "gray85"))))
 '(custom-comment-face ((((class grayscale color) (background light)) (:inherit custom-documentation-face :background "gray85"))) t)
 '(custom-comment-tag ((((class color) (background light)) (:inherit custom-documentation :foreground "blue4"))))
 '(custom-comment-tag-face ((((class color) (background light)) (:inherit custom-documentation-face :foreground "blue4"))) t)
 '(custom-documentation ((t (:height 90 :family "microsoft-tahoma"))))
 '(custom-documentation-face ((t (:family "microsoft-tahoma"))) t)
 '(custom-group-tag ((((min-colors 88) (class color) (background light)) (:inherit variable-pitch :foreground "blue1" :weight bold :height 1.2))))
 '(custom-group-tag-face ((((class color) (background light)) (:inherit variable-pitch :foreground "red" :weight bold :height 1.2))) t)
 '(custom-invalid-face ((((class color)) (:inherit custom-documentation-face :background "red" :foreground "yellow"))) t)
 '(custom-modified-face ((((class color)) (:inherit custom-documentation-face :background "blue" :foreground "white"))) t)
 '(custom-rogue-face ((((class color)) (:inherit custom-documentation-face :background "black" :foreground "pink"))) t)
 '(custom-saved-face ((t (:inherit custom-documentation-face :underline t))) t)
 '(custom-set-face ((((class color)) (:inherit custom-documentation-face :background "white" :foreground "blue"))) t)
 '(custom-state ((((class color) (background light)) (:inherit custom-documentation :foreground "dark green"))))
 '(custom-state-face ((((class color) (background light)) (:inherit custom-documentation-face :foreground "dark green"))) t)
 '(diff-added ((t (:inherit diff-changed :background "#EEFFEE" :foreground "#009900"))))
 '(diff-changed ((nil (:background "grey95"))))
 '(diff-context ((((class color grayscale) (min-colors 88)) (:inherit shadow :foreground "#333333"))))
 '(diff-file-header ((((class color) (min-colors 88) (background light)) (:weight bold))))
 '(diff-header ((((class color) (min-colors 88) (background light)) (:foreground "#3333FF"))))
 '(diff-hunk-header ((t (:background "#eeeeee" :weight bold))))
 '(diff-indicator-added ((t (:inherit diff-added :weight bold))))
 '(diff-indicator-removed ((t (:inherit diff-removed :weight bold))))
 '(diff-removed ((t (:inherit diff-changed :background "#FFEEEE" :foreground "#990000"))))
 '(ecb-default-general-face ((((class color) (background light)) (:height 100 :family "microsoft-tahoma"))))
 '(ecb-default-highlight-face ((((class color) (background light)) (:inherit ecb-default-general-face :background "cornflower blue" :foreground "yellow" :family "microsoft-tahoma"))))
 '(ecb-tag-header-face ((((class color) (background light)) (:background "SeaGreen1"))))
 '(ecb-tree-highlight-face ((((class color) (background light)) (:inherit (ecb-default-general-face highlight) :height 1.0))))
 '(file-name-shadow ((t (:inherit shadow :foreground "grey80"))))
 '(fixed-pitch ((t nil)))
 '(flyspell-duplicate ((t (:foreground "Gold3" :underline t))))
 '(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t))))
 '(font-latex-verbatim-face ((((class color) (background light)) (:inherit monotype-courier\ new :foreground "SaddleBrown"))))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face :slant normal)) (((class color) (min-colors 16)) nil)))
 '(font-lock-comment-face ((t (:foreground "#0a0" :slant oblique :height 1.0))))
 '(font-lock-doc-face ((t (:foreground "#005" :slant italic :weight normal))))
 '(font-lock-function-name-face ((t (:foreground "Blue" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "Purple" :weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "#999"))))
 '(font-lock-string-face ((t (:foreground "firebrick" :height 1.0))))
 '(font-lock-type-face ((t (:foreground "#d00" :underline nil :weight bold))))
 '(font-lock-warning-face ((((class color) (min-colors 88) (background light)) (:background "yellow" :foreground "Red1" :slant normal :weight extra-bold))))
 '(fringe ((((class color) (background light)) (:background "grey96"))))
 '(grep-edit-face ((t (:background "#77ff55" :weight bold))))
 '(grep-edit-file-face ((t (:background "#77ff55" :weight bold))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "#FAFABF"))))
 '(ido-first-match ((t (:inherit font-lock-function-name-face))))
 '(ido-first-match-face ((t (:inherit font-lock-function-name-face))))
 '(ido-only-match-face ((((class color)) (:inherit font-lock-comment-face))))
 '(margin-face ((t (:background "red"))) t)
 '(minibuffer-prompt ((t (:foreground "dark blue"))))
 '(mode-line ((t (:background "#FFBB44" :foreground "black" :box (:line-width 3 :color "#FFBB44") :height 99 :family "microsoft-tahoma"))))
 '(mode-line-buffer-id ((t (:foreground "#990000" :weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88) (background light)) (:inherit mode-line :background "RoyalBlue4" :foreground "white" :box (:line-width 2 :color "RoyalBlue4")))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey80" :foreground "grey20" :box (:line-width 3 :color "grey80")))))
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-background-chunk-submode ((((class color) (min-colors 88) (background light)) (:background "#efefff"))))
 '(nxml-comment-content-face ((t (:inherit font-lock-comment-face))))
 '(org-special-keyword ((((class color) (min-colors 16) (background light)) (:foreground "#66aa00"))))
 '(pesche-tab ((t (:background "red"))))
 '(py-XXX-tag-face ((t (:background "yellow" :foreground "#f00"))) t)
 '(py-builtins-face ((t (:inherit font-lock-keyword-face :weight normal))) t)
 '(py-pseudo-keyword-face ((t (:inherit font-lock-keyword-face :weight normal))) t)
 '(semantic-dirty-token-face ((((class color) (background light)) (:background "gray96"))))
 '(semantic-highlight-edits-face ((((class color) (background light)) (:background "gray95"))))
 '(semantic-unmatched-syntax-face ((((class color) (background light)) (:underline "red"))))
 '(show-ws-spaces ((((class color)) nil)))
 '(show-ws-tabs ((((class color)) (:inherit trailing-whitespace))))
 '(show-ws-unbr-spaces ((((class color)) nil)))
 '(speedbar-directory-face ((((class color) (background light)) (:inherit speedbar-file-face :foreground "blue4"))))
 '(speedbar-file-face ((((class color) (background light)) (:foreground "cyan4" :family "microsoft-tahoma"))))
 '(speedbar-highlight-face ((((class color) (background light)) (:inherit speedbar-file-face :background "green"))))
 '(speedbar-selected-face ((((class color) (background light)) (:inherit speedbar-file-face :foreground "red" :underline t))))
 '(speedbar-tag-face ((((class color) (background light)) (:inherit speedbar-file-face :foreground "brown"))))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "dark red"))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default :background "white" :box (:line-width 2 :color "white")))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray90" :foreground "gray50" :box (:line-width 3 :color "gray90") :height 99))))
 '(tabbar-highlight ((t (:underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "blue" :weight bold))))
 '(tabbar-separator ((t (:inherit tabbar-default))))
 '(tabbar-unselected ((t (:inherit tabbar-default))))
 '(table-cell-face ((t (:foreground "#0000aa" :inverse-video nil))))
 '(tool-bar ((default (:foreground "black" :box (:line-width 1 :style released-button))) (((type x w32 mac) (class color)) (:background "grey75"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "#ffcccc"))))
 '(trex-unicode-face ((t nil)))
 '(variable-pitch ((t (:height 105 :family "microsoft-tahoma"))))
 '(widget-documentation ((((class color) (background light)) (:inherit custom-documentation :foreground "dark green"))))
 '(woman-addition ((t (:inherit default :foreground "orange"))))
 '(woman-bold ((((min-colors 88) (background light)) (:inherit default :foreground "blue1" :weight bold))))
 '(woman-italic ((((min-colors 88) (background light)) (:inherit default :foreground "red1" :underline t :slant italic))))
 '(woman-unknown ((((background light)) (:inherit default :foreground "brown")))))


;; ---------- Font height switch -----------------------------------------------

;; set font height depending on machine -- currently crashes Emacs
;(if (equal system-name "r106120.ga1.swh.mhn.de")
;    (set-face-attribute 'default nil :height 105)
;  (set-face-attribute 'default nil :height 94))
