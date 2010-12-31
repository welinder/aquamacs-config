(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system
(require 'slime) ; assume slime is already in aquamacs load path
(slime-setup '(slime-repl))
