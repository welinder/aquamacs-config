# Aquamacs Emacs Configuration #

This is my Aquamacs Emacs configuration. Pieces are stolen and modified from all over, but some common sources are:

* https://github.com/bashu/aquamacs-starter-kit
* https://github.com/gregnewman/20seven-emacs

# Installation Instructions #

1. Add the following to `/Library/Preferences/Aquamacs Emacs/Preferences.el`:

    (setq config-files-dir (concat (file-name-directory
      (or (buffer-file-name) load-file-name)) "/aquamacs-config"))
    (add-to-list 'load-path config-files-dir)
    (require 'init)

2. Make sure your Python configuration works by checking instructions in `custom/python.el`.
