jshint-mode
=======

Integrate [jshint](http://jshint.com) into emacs via a node.js server, heavily inspired by Kevin Turners [lintnode](https://github.com/keturn/lintnode) much thanks.

![example](https://github.com/daleharvey/jshint-mode/raw/master/example.png)

Dependencies
============

[node.js](http://nodejs.org/)

Building
========

Install via [npm](http://npmjs.org/)

    $ npm install jshint-mode

or you can [download](https://github.com/daleharvey/jshint-mode/tarball/master) or clone

    $ git clone git://github.com/daleharvey/jshint-mode.git

Usage
=====

Add the configuration to your .emacs config file:

    $jshint-emacs-path >> ~/.emacs

or if you downloaded manually:

    (add-to-list 'load-path "~/path/to/jshint-mode")
    (require 'flymake-jshint)
    (add-hook 'javascript-mode-hook
         (lambda () (flymake-mode t)))

You can use M-x flymake-mode to turn flymake of and on, if you want to turn it on be fault, add the following to your .emacs

    ;; Turns on flymake for all files which have a flymake mode
    (add-hook 'find-file-hook 'flymake-find-file-hook)

To run from the command line:

    $ node jshint-mode.js
    $ curl --form source="<path/to/my.js" --form=filename="my.js" http://127.0.0.1:3003/jshint
