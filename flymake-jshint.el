;; adapted from http://www.emacswiki.org/emacs/FlymakeJavaScript
;;
;; Installation:
;;
;;     (add-to-list 'load-path "~/lib/jshint-mode")
;;     (require 'flymake-jshint)
;;     (add-hook 'javascript-mode-hook
;;         (lambda () (flymake-mode t)))
;;
;; run M-x flymake-mode to turn flymake on and off
;;

(require 'flymake)

(defcustom jshint-mode-node-program "node"
  "The program name to invoke node.js."
  :type 'string
  :group 'flymake-jshint)

(defcustom jshint-mode-location "~/.emacs.d/jshint-mode"
  "The directory jshint-mode.js may be found in."
  :type 'string
  :group 'flymake-jshint)

(defcustom jshint-mode-port 3003
  "The port the jshint-mode server runs on."
  :type 'integer
  :group 'flymake-jshint)

;; doesnt currently work
;; (defun jshint-mode-start ()
;;   "Start the jshint-mode server.
;; Uses `jshint-mode-node-program' and `jshint-mode-location'."
;;   (interactive)
;;   (start-process "jshint-mode-server" "*jshint-mode*"
;;                  jshint-mode-node-program (concat jshint-mode-location "/jshint-mode.js")
;;                  "--port" (number-to-string jshint-mode-port)))

(defun flymake-jshint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
         (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name)))
         (jshint-url (format "http://127.0.0.1:%d/jshint" jshint-mode-port)))
    (list "curl" (list "--form" (format "source=<%s" local-file)
                       "--form" (format "filename=%s" local-file)
                       jshint-url))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.js$"
	      flymake-jshint-init
	      flymake-simple-cleanup
	      flymake-get-real-file-name)
	    flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"
	      nil 1 2 3)
	    flymake-err-line-patterns))

(provide 'flymake-jshint)
