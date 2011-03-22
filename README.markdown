jshint-mode
=======

Integrate jshint into emacs via a node.js server, heavily inspited by Kevin Turners lintnode (https://github.com/keturn/lintnode) much thanks.

Dependencies
============

node.js (http://nodejs.org/)

Building
========

Download
    https://github.com/daleharvey/jshint-mode/tarball/master
or
    $ git clone git://github.com/daleharvey/jshint-mode.git

Usage
=====

To run within emacs (add this to your .emacs) run M-x flymake-mode to turn jshint on and off

     (add-to-list 'load-path "~/path/to/jshint-mode")
     (require 'flymake-jshint)
     (add-hook 'javascript-mode-hook
         (lambda () (flymake-mode t)))

     ;; Turns on flymake for all files which have a flymake mode
     (add-hook 'find-file-hook 'flymake-find-file-hook)

To run from the command line:

    $ node jshint-mode.js
    $ curl --form source="<path/to/my.js" --form=filename="my.js" http://127.0.0.1:3003/jshint
