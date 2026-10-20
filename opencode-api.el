;;; opencode-api.el --- Wrapper for opencode api     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Wrapper for opencode api

;;; Code:

(require 'opencode-common)
(require 'plz)
(require 'json)
(require 'url-util)

(defvar opencode-api-url nil
  "URL of the opencode server we're connected to.")

(defvar opencode-api-password nil
  "Password used for HTTP Basic auth with the opencode service.")

(defvar opencode-api-log-max-lines nil
  "Maximum number of lines of opencode api requests and responses to log.
Or nil (default) to turn off logging.")

(defun opencode--auth-header ()
  "Return a Basic auth header using `opencode-api-password'."
  (when opencode-api-password
    (cons "Authorization"
          (concat "Basic "
                  (base64-encode-string (concat "opencode:" opencode-api-password))))))

(defun opencode-api--add-location-query (path kind)
  "Append a location-scoped query derived from `default-directory' to PATH.
KIND is `:location' to add `location[directory]=...', `:directory' to add
`directory=...', or nil to leave PATH unchanged."
  (if-let ((param (pcase kind
                    (:location "location%5Bdirectory%5D=")
                    (:directory "directory="))))
      (concat path (if (string-match-p "\\?" path) "&" "?")
              param (url-hexify-string (directory-file-name (expand-file-name default-directory))))
    path))

(defun opencode-api--unwrap-data (result)
  "Return the `data' field of RESULT when present, else RESULT itself."
  (if-let ((entry (and (listp result) (assq 'data result))))
      (cdr entry)
    result))

(eval-and-compile
  (cl-defun opencode-api--call (method path return-var body &key data location)
    "Generate the body of an opencode api wrapper macro.
Using METHOD, for endpoint PATH, saving result in RETURN-VAR,
and saving to CURRENT-BUFFER while running BODY. DATA is the request body
form, if any. LOCATION is `:location', `:directory', or nil, controlling
which location query parameter (if any) is appended to PATH."
    (cl-with-gensyms (current-buffer result saved-path saved-data)
      `(let* ((,current-buffer (current-buffer))
              (,saved-path (opencode-api--add-location-query (concat "/api" ,path) ,location))
              (,saved-data ,data)
              ;; plz 0.9.1 has no PATCH and feeds the body to curl as config
              ;; (exit 26); pass method and body as curl args until plz 0.10.
              ,@(when (eq method 'patch)
                  `((plz-curl-default-args
                     (append plz-curl-default-args
                             (list "--request" "PATCH"
                                   "--data-binary" (json-encode ,saved-data)))))))
         (when opencode-api-log-max-lines
           (with-current-buffer (get-buffer-create "*opencode-api-log*")
             (save-excursion
               (goto-char (point-max))
               (insert "REQUEST: " ,saved-path "\n")
               (when ,saved-data
                 (insert "REQUEST BODY:")
                 (pp ,saved-data (current-buffer)))
               (opencode--truncate-at-max-lines opencode-api-log-max-lines))))
         (plz ',(if (eq method 'patch) 'get method) (concat opencode-api-url ,saved-path)
           :as (lambda () (unless (string-empty-p (buffer-string))
                            (json-parse-buffer :array-type 'list
                                               :object-type 'alist)))
           :headers `(("Content-Type" . "application/json")
                      ,(opencode--auth-header))
           ,@(when (and data (not (eq method 'patch)))
               `(:body (json-encode ,saved-data)))
           :then (lambda (,result)
                   (when opencode-api-log-max-lines
                     (with-current-buffer
                         (get-buffer-create "*opencode-api-log*")
                       (save-excursion
                         (goto-char (point-max))
                         (insert "RESPONSE: ")
                         (pp ,result (current-buffer)))))
                   (let ((,return-var (opencode-api--unwrap-data ,result)))
                     (if (buffer-live-p ,current-buffer)
                         (with-current-buffer ,current-buffer
                           ,@body)
                       (message "Buffer closed so opencode api response handler not run"))))
           :else (lambda (response)
                   (let ((error-msg (format "error requesting %s: %s" ,saved-path response)))
                     (when opencode-api-log-max-lines
                       (with-current-buffer
                           (get-buffer-create "*opencode-api-log*")
                         (save-excursion
                           (goto-char (point-max))
                           (insert "ERROR: " error-msg "\n"))))
                     (if opencode--event-subscription
                         (error error-msg)
                       (error "Not connected to opencode"))))))))

  (cl-defun opencode-api--wrap (method path &key elisp-macro-name nodata location)
    "Define a macro to wrap api call with METHOD and PATH.
optionally provide ELISP-MACRO-NAME where opencode-api-elisp-macro-name will be
the name of the created macro. Set NODATA to indicate a request doesn't have a
body when it normally would (POST PATCH). LOCATION is `:location', `:directory',
or nil, see `opencode-api--add-location-query'."
    (let ((elisp-macro-name
           (intern
            (concat "opencode-api"
                    (if elisp-macro-name
                        (concat "-" (symbol-name elisp-macro-name))
                      (string-replace "/" "-" path))))))
      (eval
       (if (seq-contains-p path ?\%)
           (if (and (seq-contains-p '(post patch) method)
                    (not nodata))
               `(defmacro ,elisp-macro-name
                    (args data return-var &rest body)
                  ,(format "Wrapper for opencode %s endpoint." path)
                  (declare (indent 3))
                  (opencode-api--call ',method `(format ,,path ,@args) return-var body
                                      :data data :location ',location))
             `(defmacro ,elisp-macro-name
                  (args return-var &rest body)
                ,(format "Wrapper for opencode %s endpoint." path)
                (declare (indent 2))
                (opencode-api--call ',method `(format ,,path ,@args) return-var body
                                    :location ',location)))
         (if (and (seq-contains-p '(post patch) method)
                  (not nodata))
             `(defmacro ,elisp-macro-name
                  (data return-var &rest body)
                ,(format "Wrapper for opencode %s endpoint." path)
                (declare (indent 2))
                (opencode-api--call ',method ,path return-var body
                                    :data data :location ',location))
           `(defmacro ,elisp-macro-name
                (return-var &rest body)
              ,(format "Wrapper for opencode %s endpoint." path)
              (declare (indent defun))
              (opencode-api--call ',method ,path return-var body :location ',location)))))))

  (defun opencode-api-define-wrapper (endpoint)
    "Create a macro wrapping ENDPOINT."
    (let ((method 'get)
          nodata
          location)
      (when (listp endpoint)
        (when (seq-contains-p '(get patch post delete) (car endpoint))
          (setf method (pop endpoint)))
        (when (eq 'nodata (car endpoint))
          (pop endpoint)
          (setf nodata t))
        (when (keywordp (car (last endpoint)))
          (setf location (car (last endpoint)))
          (setf endpoint (butlast endpoint))))
      (pcase endpoint
        ((pred stringp) (opencode-api--wrap method endpoint :nodata nodata :location location))
        (`(,elisp-name ,path) (opencode-api--wrap method path
                                                  :elisp-macro-name elisp-name
                                                  :nodata nodata
                                                  :location location)))))

  (defun opencode-api-define-wrappers (api-endpoints)
    "Define wrapping macros for a list of API-ENDPOINTS."
    (dolist (api-endpoint api-endpoints)
      (opencode-api-define-wrapper api-endpoint)))

  (opencode-api-define-wrappers
   '("/info"
     (projects "/project")
     (location "/location" :location)
     (models "/model" :location)
     (agents "/agent" :location)
     (commands "/command" :location)
     (mcps "/mcp" :location)
     (post nodata connect-mcp "/experimental/mcp/%s/connect" :location)
     (post nodata disconnect-mcp "/experimental/mcp/%s/disconnect" :location)
     (sessions "/session" :directory)
     (session-children "/session?parentID=%s")
     (sessions-active "/session/active")
     (session "/session/%s")
     (post create-session "/session")
     (patch rename-session "/session/%s")
     (delete delete-session "/session/%s")
     (session-messages "/session/%s/message")
     (post send-message "/session/%s/prompt")
     (post set-session-model "/session/%s/model")
     (post set-session-agent "/session/%s/agent")
     (post nodata interrupt-session "/session/%s/interrupt")
     (post fork-session "/session/%s/fork")
     (post revert-stage "/session/%s/revert/stage")
     (post nodata revert-commit "/session/%s/revert/commit")
     (delete revert-clear "/session/%s/revert")
     (post execute-command "/session/%s/command")
     (post execute-shell "/session/%s/shell")
     (post nodata compact-session "/session/%s/compact")
     (session-permissions "/session/%s/permission")
     (post reply-permission "/session/%s/permission/%s/reply")
     (session-forms "/session/%s/form")
     (post reply-form "/session/%s/form/%s/reply")
     (delete cancel-form "/session/%s/form/%s"))))

(provide 'opencode-api)
;;; opencode-api.el ends here
