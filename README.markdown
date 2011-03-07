jshint-mode
=======

A work in progress to integrate jshint into emacs via a node.js server, heavily inspited by Kevin Turners lintnode (https://github.com/keturn/lintnode) much thanks.

Dependencies
============

node.js (http://nodejs.org/)

Building
========

    git clone git://github.com/daleharvey/jshint-mode.git

Usage
=====

To run from the command line:

    $ node jshint-mode.js
    $ curl --form source="<path/to/my.js" --form=filename="my.js" http://127.0.0.1:3003/jshint

To run within emacs (add this to your .emacs)

     (add-to-list 'load-path "~/lib/jshint-mode")
     (require 'flymake-jshint)
     (add-hook 'javascript-mode-hook
         (lambda () (flymake-mode t)))

You need to start the node server manually

    $ node jshint-mode.js

and then run M-x flymake-mode to turn jshint on and off
