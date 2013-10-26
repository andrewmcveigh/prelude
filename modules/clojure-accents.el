(require 'clojure-mode)
(require 'cider)
(require 'cider-interaction)
(require 'nrepl-client)

(defun string/join (sep strings)
  (mapconcat 'identity strings sep))

(defun accent/nrepl-eval-request (input &optional ns session)
  "Send a request to eval INPUT.
If NS is non-nil, include it in the request.
Use SESSION if it is non-nil, otherwise use the current session."
  (append (if ns (list "ns" ns))
          (list
           "op" "eval"
           "session" (or session (nrepl-current-session))
           "code" input
           "buffer" (plist-get buffer-meta 'name)
           "path" (plist-get buffer-meta 'filename)
           "accent" (plist-get buffer-meta 'filetype))))

(defun accent/nrepl-send-string (input callback &optional ns session)
  "Send the request INPUT and register the CALLBACK as the response handler.
See command `nrepl-eval-request' for details on how NS and SESSION are processed."
  (let ((ns (if (string-match "[[:space:]]*\(ns\\([[:space:]]*$\\|[[:space:]]+\\)" input)
                "user"
              ns)))
    (nrepl-send-request (accent/nrepl-eval-request input ns session) callback)))

(defun accent/set-buffer-meta ()
  (let* ((file (buffer-name))
         (coll (split-string (clojure-expected-ns) "\\."))
         (ns (string/join "." (if (string-match "^clj" (car coll))
                                  (cdr coll)
                                coll))))
    (setq buffer-meta (plist-put buffer-meta 'name file))
    (setq buffer-meta (plist-put buffer-meta 'filename (buffer-file-name)))
    (setq buffer-meta (plist-put buffer-meta 'ns ns))
    (cond ((string-match "\.cljs$" file)
           (setq buffer-meta (plist-put buffer-meta 'filetype "cljs")))
          ((string-match "\.clj$" file)
           (setq buffer-meta (plist-put buffer-meta 'filetype "clj")))
          ((string-match "\.cljx$" file)
           (setq buffer-meta (plist-put buffer-meta 'filetype "cljx"))))))

(define-minor-mode clojure-accents-mode
  "Clj/Cljs/Cljx interaction and co-development."
  :lighter " clj^"
  :keymap (progn
            (evil-make-overriding-map clojure-mode-map 'normal t)
            (evil-define-key 'normal clojure-mode-map
              "mm" (lambda ()
                     (interactive)
                     (message "%s" buffer-meta))
              "mpp" (lambda ()
                      (interactive)
                      (accent/nrepl-send-string "(+ 1 2)"
                                                (lambda (&rest args)
                                                  (message args))
                                                "user"
                                                nil)))
            clojure-mode-map)
  (make-variable-buffer-local
   (defvar buffer-meta '()
     "Metadata about the Clojure^ buffer."))
  (accent/set-buffer-meta))

(add-hook 'clojure-mode-hook 'clojure-accents-mode)

(provide 'clojure-accents)
