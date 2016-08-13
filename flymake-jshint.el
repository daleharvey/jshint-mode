;;; flymake-jshint.el --- JSHint mode for Emacs
;;
;; Version: 20110901
;;
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

(defcustom jshint-mode-mode "jshint"
  "Can use either jshint or jslint"
  :type 'string
  :group 'flymake-jshint)

(defcustom jshint-mode-node-program "node"
  "The program name to invoke node.js."
  :type 'string
  :group 'flymake-jshint)

(defcustom jshint-mode-location (file-name-directory load-file-name)
  "The directory jshint-mode.js may be found in."
  :type 'string
  :group 'flymake-jshint)

(defcustom jshint-mode-port 3003
  "The port the jshint-mode server runs on."
  :type 'integer
  :group 'flymake-jshint)

(defcustom jshint-mode-lastport 3100
  "Defines the higher end of the port range to run on."
  :type 'integer
  :group 'flymake-jshint)

(defcustom jshint-mode-host "127.0.0.1"
  "The host the jshint-mode server runs on."
  :type 'string
  :group 'flymake-jshint)

(defcustom jshint-mode-jshintrc ""
  "Location for the jshintrc file. If not set, looks up directory tree from each JS file."
  :type 'string
  :group 'flymake-jshint)

(setq jshint-process "jshint-mode-server")
(setq jshint-buffer "*jshint-mode*")
(setq jshint-dynamic-port nil) ;; server prints on stdout. we grep for it
(setq jshint-process-output nil)
(setq jshint-start-regex "\\`Started[^:]+://\\([^:]+\\):\\([0-9]+\\)[\.]")

;; The server might try different ports before starting.
;; We locate the port in the stdout messages.
(defun jshint-extract-port-filter (proc string)
  ;; the following block just adds the output to the process buffer
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (insert string)
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc))))))

  (when (eq jshint-dynamic-port nil)
    (setq jshint-process-output (concat jshint-process-output string)) ;; all text received so far
    (save-match-data
      (when (string-match jshint-start-regex jshint-process-output)
	(setq jshint-dynamic-port (string-to-number (match-string 2 jshint-process-output)))
	(message
	 (concat "jshint server has started on " jshint-mode-host ":"
		 (number-to-string jshint-dynamic-port)))
	(set-process-filter (get-process jshint-process) nil)))))

(defun jshint-mode-init ()
  "Start the jshint-mode server."
  (interactive)
  (if (eq (process-status jshint-process) 'run)
      (if (not (eq jshint-dynamic-port nil))
	  'started
	'starting)
    (start-process
     jshint-process
     jshint-buffer
     jshint-mode-node-program
     (expand-file-name (concat jshint-mode-location "/jshint-mode.js"))
     "--host" jshint-mode-host
     "--port" (number-to-string jshint-mode-port)
     "--lastport" (number-to-string jshint-mode-lastport))
    (set-process-filter (get-process jshint-process) 'jshint-extract-port-filter)
    (set-process-query-on-exit-flag (get-process jshint-process) nil)
    'starting
    ))

(defun jshint-mode-stop ()
  "Stop the jshint-mode server."
  (interactive)
  (delete-process jshint-process))

(defun flymake-jshint-init ()
  (if (eq (jshint-mode-init) 'started)
      (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
             (local-file (file-relative-name temp-file
                                             (file-name-directory buffer-file-name)))
             (jshint-url (format "http://%s:%d/check" jshint-mode-host jshint-dynamic-port))
             (jshintrc (if (string= "" jshint-mode-jshintrc)
                           (expand-file-name
                            ".jshintrc"
                            (locate-dominating-file default-directory ".jshintrc"))
                         jshint-mode-jshintrc)))
        (list "curl" (list "--form" (format "source=<%s" local-file)
                           "--form" (format "filename=%s" local-file)
                           "--form" (format "mode=%s" jshint-mode-mode)
                           "--form" (format "jshintrc=%s" jshintrc)
                           jshint-url)))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.js\\(\\.in\\)?$"
	      flymake-jshint-init
	      flymake-simple-cleanup
	      flymake-get-real-file-name)
	    flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"
	      nil 1 2 3)
	    flymake-err-line-patterns))

(provide 'flymake-jshint)

;;; flymake-jshint.el ends here
