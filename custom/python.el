
;;; python-mode
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'python-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (set-variable 'py-indent-offset 4)
            ;(set-variable 'py-smart-indentation nil)
            (set-variable 'indent-tabs-mode nil)
            (define-key python-mode-map (kbd "RET") 'newline-and-indent)
            ;(define-key py-mode-map [tab] 'yas/expand)
            ;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
	          ;(smart-operator-mode-on)
            (define-key python-mode-map "\"" 'electric-pair)
            (define-key python-mode-map "\'" 'electric-pair)
            (define-key python-mode-map "(" 'electric-pair)
            (define-key python-mode-map "[" 'electric-pair)
            (define-key python-mode-map "{" 'electric-pair)
            ))
(defun electric-pair ()
  "Insert character pair without surrounding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

;;; IPython Integration
; More about getting EDITOR to work here:http://ipython.scipy.org/doc/manual/html/config/editors.html
(setq ipython-command 
  "/Library/Frameworks/EPD64.framework/Versions/Current/bin/ipython")
(require 'ipython)
(setq py-python-command-args '( "-pdb" ))
;; TODO: the following doesn't work...?
(defadvice py-execute-buffer (around python-keep-focus activate)
  "return focus to python code buffer"
  (save-excursion ad-do-it))
;; nicer access to command history
(require 'comint)
(define-key comint-mode-map (kbd "M-p") 'comint-previous-matching-input-from-input)
(define-key comint-mode-map (kbd "M-n") 'comint-next-matching-input-from-input)
(define-key comint-mode-map (kbd "C-M-p") 'comint-next-input)
(define-key comint-mode-map (kbd "C-M-p") 'comint-previous-input)


;;; Pymacs python integration
;; Read more here: http://pymacs.progiciels-bpi.ca/pymacs.html
;; Pymacs Installation Guide
;; 1. Get Pymacs: git clone https://github.com/pinard/Pymacs.git
;; 2. Do `make check` -- all test should go fine
;; 3. Do `make install`
;; 4. See that it works by doing `from Pymacs import lisp` from python shell.
;; 5. Place `pymacs.el` in the `plugins/` directory.
;; 6. For speed, byte compile the file: `M-x byte-compile-file RET pymacs.el RET`
;; 7. You should now be able to run `M-x pymacs-eval`

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; TODO: create your own Pymacs directory to keep your Python code, then use these:
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
(require 'pymacs)

;;; Ropemacs
;; Installation: Run `python setup.py install` from the archive in src/
(pymacs-load "ropemacs" "rope-")


;;; Syntax Check using Flymake to use PyFlakes.
;; http://plope.com/Members/chrism/flymake-mode
;; http://flymake.sourceforge.net/
(when (load "flymake" t) 
  (defun flymake-pyflakes-init () 
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                       'flymake-create-temp-inplace)) 
           (local-file (file-relative-name 
                        temp-file 
                        (file-name-directory buffer-file-name)))) 
      (list "pyflakes" (list local-file)))) 
  (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pyflakes-init))) 
(add-hook 'find-file-hook 'flymake-find-file-hook)