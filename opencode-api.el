;;; opencode-api.el --- Wrapper for opencode api     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Wrapper for opencode api

;;; Code:

(require 'opencode-common)
(require 'plz)
(require 'json)

(defvar opencode-api-url nil
  "URL of the opencode server we're connected to.")

(defvar opencode-api-log-max-lines nil
  "Maximum number of lines of opencode api requests and responses to log.
Or nil (default) to turn off logging.")

(defun opencode--auth-header ()
  "Return auth header based on `opencode-server-username' and `opencode-server-password'."
  (when opencode-server-password
    (cons "Authorization"
          (concat "Basic "
                  (base64-encode-string
                   (format "%s:%s" opencode-server-username opencode-server-password))))))

(eval-and-compile
  (cl-defun opencode-api--call (method path return-var body &key data)
    "Generate the body of an opencode api wrapper macro.
Using METHOD, for endpoint PATH, saving result in RETURN-VAR,
and saving to CURRENT-BUFFER while running BODY."
    (cl-with-gensyms (current-buffer result saved-path saved-data)
      `(let ((,current-buffer (current-buffer))
             (,saved-path ,path)
             (,saved-data ,data))
         (when opencode-api-log-max-lines
           (with-current-buffer (get-buffer-create "*opencode-api-log*")
             (save-excursion
               (goto-char (point-max))
               (insert "REQUEST: " ,saved-path "\n")
               (when ,saved-data
                 (insert "REQUEST BODY:")
                 (pp ,saved-data (current-buffer)))
               (opencode--truncate-at-max-lines opencode-api-log-max-lines))))
         (plz ',method (concat opencode-api-url ,saved-path)
           :as (lambda () (unless (string-empty-p (buffer-string))
                       (json-parse-buffer :array-type 'list
                                          :object-type 'alist)))
           :headers `(("Content-Type" . "application/json")
                      ,(cons "x-opencode-directory" default-directory)
                      ,(opencode--auth-header))
           ,@(when data
               `(:body (json-encode ,saved-data)))
           :then (lambda (,result)
                   (when opencode-api-log-max-lines
                     (with-current-buffer
                         (get-buffer-create "*opencode-api-log*")
                       (save-excursion
                         (goto-char (point-max))
                         (insert "RESPONSE: ")
                         (pp ,result (current-buffer)))))
                   (let ((,return-var ,result))
                     (if (buffer-live-p ,current-buffer)
                         (with-current-buffer ,current-buffer
                           ,@body)
                       ,@body)))
           :else (lambda (response)
                   (let ((error-msg (format "error requesting %s: %s" ,saved-path response)))
                     (when opencode-api-log-max-lines
                       (with-current-buffer
                           (get-buffer-create "*opencode-api-log*")
                         (save-excursion
                           (goto-char (point-max))
                           (insert "ERROR: " error-msg "\n"))))
                     (if opencode--event-subscriptions
                         (error error-msg)
                       (error "Not connected to opencode"))))))))

  (cl-defun opencode-api--wrap (method path &key elisp-macro-name nodata)
    "Define a macro to wrap api call with METHOD and PATH.
optionally provide ELISP-MACRO-NAME where opencode-api-elisp-macro-name will be
the name of the created macro. Set NODATA to indicate a request doesn't have a
body when it normally would (POST PATCH)."
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
                                      :data data))
             `(defmacro ,elisp-macro-name
                  (args return-var &rest body)
                ,(format "Wrapper for opencode %s endpoint." path)
                (declare (indent 2))
                (opencode-api--call ',method `(format ,,path ,@args) return-var body)))
         (if (and (seq-contains-p '(post patch) method)
                  (not nodata))
             `(defmacro ,elisp-macro-name
                  (data return-var &rest body)
                ,(format "Wrapper for opencode %s endpoint." path)
                (declare (indent 2))
                (opencode-api--call ',method ,path return-var body
                                    :data data))
           `(defmacro ,elisp-macro-name
                (return-var &rest body)
              ,(format "Wrapper for opencode %s endpoint." path)
              (declare (indent defun))
              (opencode-api--call ',method ,path return-var body)))))))

  (defun opencode-api-define-wrapper (endpoint)
    "Create a macro wrapping ENDPOINT."
    (let ((method 'get)
          nodata)
      (when (and (listp endpoint)
                 (seq-contains-p '(get patch post delete) (car endpoint)))
        (setf method (pop endpoint))
        (when (eq 'nodata (car endpoint))
          (pop endpoint)
          (setf nodata t)))
      (pcase endpoint
        ((pred stringp) (opencode-api--wrap method endpoint :nodata nodata))
        (`(,elisp-name ,path) (opencode-api--wrap method path
                                                  :elisp-macro-name elisp-name
                                                  :nodata nodata)))))

  (defun opencode-api-define-wrappers (api-endpoints)
    "Define wrapping macros for a list of API-ENDPOINTS."
    (dolist (api-endpoint api-endpoints)
      (opencode-api-define-wrapper api-endpoint)))

  (opencode-api-define-wrappers
   '("/global/health"
     (projects "/project")
     (current-project "/project/current")
     "/path"
     "/vcs"
     "/config"
     (configured-providers "/config/providers")
     (providers "/provider")
     (provider-authentication-methods "/provider/auth")
     (session "/session/%s")
     (session-children "/session/%s/children")
     (session-todos "/session/%s/todo")
     (session-diff "/session/%s/diff")
     (session-messages "/session/%s/message")
     (post sync-message "/session/%s/message")
     (post send-message "/session/%s/prompt_async")
     (message-details "/session/%s/message/%s")
     (sessions "/session")
     (sessions-status "/session/status")
     (post create-session "/session")
     (patch rename-session "/session/%s")
     (post nodata abort-session "/session/%s/abort")
     (delete delete-session "/session/%s")
     (post fork-session "/session/%s/fork")
     (post revert-message "/session/%s/revert")
     (post nodata unrevert-all "/session/%s/unrevert")
     (post respond-permission-request "/session/%s/permissions/%s")
     (post nodata share-session "/session/%s/share")
     (delete unshare-session "/session/%s/share")
     (post execute-command "/session/%s/command")
     (post reply-questions "/question/%s/reply")
     (post nodata reject-questions "/question/%s/reject")
     (commands "/command")
     "/file/status"
     (find-pattern "/find?pattern=%s")
     (find-files "/find/file?query=%s")
     (find-symbols "/find/symbol?query=%s")
     (list-files "/file?path=%s")
     (read-file "/file/content?path=%s")
     "/experimental/tool/ids"
     (tools-for-model "/experimental/tool?provider=%s&model=%s")
     "/lsp"
     "/formatter"
     (mcps "/mcp")
     (post nodata enable-mcp "/mcp/%s/connect")
     (post nodata disable-mcp "/mcp/%s/disconnect")
     (agents "/agent")
     (post nodata dispose-instance "/instance/dispose"))))

(provide 'opencode-api)
;;; opencode-api.el ends here
