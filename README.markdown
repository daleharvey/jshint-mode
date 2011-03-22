jshint-mode
=======

Integrate jshint into emacs via a node.js server, heavily inspited by Kevin Turners [lintnode](https://github.com/keturn/lintnode) much thanks.

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

If you installed via [npm](http://npmjs.org/) you need to find the path that jshint was installed to:

    $ npm explore jshint-mode
    Exploring /usr/local/lib/node/.npm/jshint-mode/active/package

Add this to your .emacs config file (if you didnt install via npm, replace the path):

    (add-to-list 'load-path "/usr/local/lib/node/.npm/jshint-mode/active/package")
    (require 'flymake-jshint)
    (add-hook 'javascript-mode-hook
         (lambda () (flymake-mode t)))

    ;; Turns on flymake for all files which have a flymake mode
    (add-hook 'find-file-hook 'flymake-find-file-hook)

To run from the command line:

    $ node jshint-mode.js
    $ curl --form source="<path/to/my.js" --form=filename="my.js" http://127.0.0.1:3003/jshint
