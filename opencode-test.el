;;; opencode-test.el --- Event handler tests  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'opencode)

(defun opencode-test--session-requests (session-id has-buffer)
  "Return session fetch URLs requested for an execution-finished SESSION-ID event.
When HAS-BUFFER, the session has a live buffer."
  (let ((opencode-session-buffers (make-hash-table :test 'equal))
        requests)
    (cl-letf (((symbol-function 'plz)
               (lambda (_method url &rest _)
                 (when (string-match-p (concat "/api/session/" session-id "\\'") url)
                   (push url requests))))
              ((symbol-function 'opencode-session--set-status) #'ignore))
      (with-temp-buffer
        (when has-buffer
          (puthash session-id (current-buffer) opencode-session-buffers))
        (opencode--handle-session-execution-finished-event
         "session.execution.succeeded" session-id nil)))
    requests))

(ert-deftest opencode-execution-finished-unknown-session-test ()
  "Do not fetch sessions that have no buffer, they may not exist anymore."
  (should-not (opencode-test--session-requests "ses_gone" nil)))

(ert-deftest opencode-execution-finished-open-session-test ()
  "Fetch sessions with a buffer to show the finished toast."
  (should (opencode-test--session-requests "ses_open" t)))

(provide 'opencode-test)
;;; opencode-test.el ends here
