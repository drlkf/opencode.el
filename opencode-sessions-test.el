;;; opencode-sessions-test.el --- Session rendering tests  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'opencode-sessions)

(defmacro with-opencode-session-test-buffer (&rest body)
  "Run BODY in a temporary `opencode-session-mode' buffer.
BODY can refer to the buffer's process as `process'.  The default directory
is the opencode source directory, and session state is bound so that events
for session \"session\" and assistant message \"message\" render here."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq default-directory
           (file-name-directory (locate-library "opencode-sessions")))
     (opencode-session-mode)
     (let ((process (start-process "opencode-test" (current-buffer) nil))
           (opencode-session-buffers (make-hash-table :test 'equal))
           (opencode--tool-calls-displayed (make-hash-table :test 'equal))
           (opencode--tool-call-info (make-hash-table :test 'equal)))
       (puthash "session" (current-buffer) opencode-session-buffers)
       (unwind-protect
           (progn ,@body)
         (delete-process process)))))

(ert-deftest opencode-read-tool-file-location-button-test ()
  "Format read offsets as working file-and-line buttons."
  (let* ((file-directory (file-name-directory
                          (locate-library "opencode-sessions")))
         (default-directory file-directory)
         (formatted (opencode--format-tool-call
                     "read" '((path . "opencode-sessions.el")
                              (offset . 440)
                              (limit . 35)))))
    (with-temp-buffer
      (setq default-directory file-directory)
      (insert formatted)
      (opencode--buttonize-file-references (point-min) (point-max))
      (goto-char (point-min))
      (search-forward "opencode-sessions.el:440")
      (let ((button (button-at (1- (point)))))
        (should button)
        (should (equal (button-get button 'button-data)
                       (cons (expand-file-name "opencode-sessions.el"
                                               file-directory)
                             440)))))))

(ert-deftest opencode-session-markdown-fontification-test ()
  "Stream Markdown without replacing text or losing session decorations."
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode--output "# Heading\n\nSee `opencode-sessions")
    (opencode--output ".el` and *emphasis*.\n\n")
    (opencode--insert-tool-block
     "edit" '((path . "opencode-sessions.el")
              (oldString . "old") (newString . "new")))
    (opencode--output "**Testing button behavior**\n\n")
    (opencode--insert-block-with-margin
     (concat "emacs_eval_elisp:\n"
             (opencode--fenced-block "(+ 1 2)" "elisp"))
     'opencode-tool-margin-highlight)
    (opencode--maybe-insert-block-spacing)
    (opencode--show-prompt)
    (insert "pending input")
    (let ((text (buffer-substring-no-properties (point-min) (point-max)))
          (mark (marker-position (process-mark process)))
          (position (point)))
      (font-lock-ensure)
      ;; Refontifying code blocks must not clear earlier inline
      ;; Markdown, even with normal JIT font-lock enabled.
      (font-lock-flush)
      (font-lock-ensure)
      (should (equal text (buffer-substring-no-properties
                           (point-min) (point-max))))
      (should (= mark (marker-position (process-mark process))))
      (should (= position (point)))
      (should (equal "pending input" (buffer-substring-no-properties
                                      mark (point-max)))))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Heading")
      (should (memq 'markdown-header-face-1
                    (ensure-list (get-text-property (1- (point)) 'face))))
      (goto-char (point-min))
      (search-forward "opencode-sessions.el")
      (let ((button (button-at (1- (point)))))
        (should button)
        (should (equal (car (button-get button 'button-data))
                       (expand-file-name "opencode-sessions.el"))))
      (goto-char (point-min))
      (search-forward "-old")
      (should (memq 'diff-removed
                    (ensure-list (get-text-property (1- (point)) 'face))))
      (should (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                       (overlays-at (1- (point)))))
      (search-forward "```")
      (should-not (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                           (overlays-at (- (point) 2))))
      (search-forward "**Testing button behavior**")
      (should (memq 'markdown-bold-face
                    (ensure-list (get-text-property (- (point) 25) 'face))))
      (should (get-char-property (- (point) 27) 'invisible))
      (search-forward "(+ 1 2)")
      (should (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                       (overlays-at (1- (point)))))
      (search-forward "```")
      (should-not (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                           (overlays-at (- (point) 2))))
      (goto-char (point-min))
      (search-forward "> ")
      (should (equal "" (get-char-property (- (point) 2) 'display))))))

(ert-deftest opencode-session-input-deletion-at-end-test ()
  "Deleting at `point-max' in the prompt must not leave stale blockquote data."
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode--output "Some output\n\n")
    (opencode--show-prompt)
    (goto-char (point-max))
    (insert "see `foo`")
    (font-lock-ensure)
    (delete-char -1)
    (font-lock-ensure)
    (should (equal "see `foo" (buffer-substring-no-properties
                               (process-mark process) (point-max))))))

(ert-deftest opencode-session-unclosed-fence-test ()
  "Fences left open by a block must not turn later output or input into code."
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode-session--handle-event
     'session.reasoning.delta
     '((sessionID . "session") (assistantMessageID . "message") (ordinal . 0)
       (delta . "think\n```elisp\n(foo")))
    (opencode-session--handle-event
     'session.text.delta
     '((sessionID . "session") (assistantMessageID . "message") (ordinal . 1)
       (delta . "answer\n\n````sh\nls")))
    (opencode--show-prompt)
    (insert "pending")
    (font-lock-ensure)
    (should (equal "pending" (buffer-substring-no-properties
                              (process-mark process) (point-max))))
    (goto-char (point-min))
    (search-forward "answer")
    (should-not (markdown-code-block-at-pos (1- (point))))
    (search-forward "ls\n````\n")
    (should-not (markdown-code-block-at-pos (1- (point-max))))))

(ert-deftest opencode-session-tool-output-fence-test ()
  "A tool output fence ending at the process mark must not signal."
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode--maybe-insert-tool-output
     "shell" '((status . "completed") (input . ((command . "ls")))
               (content . [((type . "text") (text . "a\n\nb"))])))
    (opencode--show-prompt)
    (should (equal "> " (buffer-substring-no-properties
                         (- (point-max) 2) (point-max))))))

(ert-deftest opencode-session-unclosed-fence-before-tool-test ()
  "Fences left open by reasoning must not swallow the following tool call."
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode-session--handle-event
     'session.reasoning.delta
     '((sessionID . "session") (assistantMessageID . "message") (ordinal . 0)
       (delta . "think\n```elisp\n(foo")))
    (opencode-session--handle-event
     'session.tool.input.started '((sessionID . "session") (id . "call") (name . "shell")))
    (opencode-session--handle-event
     'session.tool.called '((sessionID . "session") (id . "call") (input . ((command . "ls")))))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "$ ls")
    (should-not (markdown-code-block-at-pos (1- (point))))))

(ert-deftest opencode-session-diff-refinement-test ()
  "Refine entire synthetic hunks without touching input or earlier output."
  (with-opencode-session-test-buffer
    (let ((diff-refine 'font-lock))
      (opencode--output "Earlier output\n")
      (insert "pending input")
      (opencode--insert-tool-block
       "apply_patch"
       '((patchText . "*** Begin Patch\n*** Update File: example.el\n@@\n-old first value\n-old second value\n+new first value\n+new second value\n@@\n-before last value\n+after last value\n*** End Patch")))
      (let ((text (buffer-substring-no-properties (point-min) (point-max)))
            (mark (marker-position (process-mark process)))
            (position (point)))
        (font-lock-ensure)
        (font-lock-flush)
        (font-lock-ensure)
        (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
        (should (= mark (process-mark process)))
        (should (= position (point)))
        (should (equal "pending input"
                       (buffer-substring-no-properties mark (point-max)))))
      (save-excursion
        (goto-char (point-min))
        (should-not (get-char-property (point) 'invisible))
        (dolist (change '(("old second" . diff-refine-removed)
                          ("new second" . diff-refine-added)
                          ("before last" . diff-refine-removed)
                          ("after last" . diff-refine-added)))
          (search-forward (car change))
          (should (cl-some
                   (lambda (overlay)
                     (eq (overlay-get overlay 'face) (cdr change)))
                   (overlays-at (- (point) (length (car change)))))))
        (goto-char (point-min))
        (while (search-forward "@@ -0 +0 @@" nil t)
          (should (get-char-property (- (point) 11) 'invisible)))))))

(ert-deftest opencode-session-yank-code-block-test ()
  "Copy only code from a fenced block, including embedded fences."
  (with-opencode-session-test-buffer
    (insert (opencode--fenced-block "```\ntext\n" "text") "\n")
    (goto-char (point-min))
    (search-forward "text" nil nil 2)
    (syntax-propertize (point-max))
    (should (markdown-code-block-at-point-p))
    (opencode-yank-code-block)
    (should (equal (current-kill 0 t) "```\ntext\n"))))

(defun opencode-sessions-test--text-delta (delta)
  "Return a session.text.delta event DATA alist streaming DELTA."
  `((sessionID . "session") (assistantMessageID . "message") (ordinal . 0)
    (delta . ,delta)))

(ert-deftest opencode-session-completed-tables-test ()
  "Align completed streamed tables once without disturbing pending input."
  (with-opencode-session-test-buffer
    (let ((align (symbol-function 'opencode--align-output-table))
          (calls 0))
      (cl-letf (((symbol-function 'opencode--align-output-table)
                 (lambda (beg end)
                   (cl-incf calls)
                   (funcall align beg end))))
        (insert "pending input")
        (opencode-session--handle-event
         'session.text.delta (opencode-sessions-test--text-delta "| File | Value |\n"))
        (opencode-session--handle-event
         'session.text.delta (opencode-sessions-test--text-delta "| :--- | ---: |\n"))
        (opencode-session--handle-event
         'session.text.delta
         (opencode-sessions-test--text-delta "| `opencode-sessions.el` | 7 |\n"))
        (should (= calls 0))
        ;; An incomplete following line cannot yet establish the boundary.
        (opencode-session--handle-event
         'session.text.delta (opencode-sessions-test--text-delta "# Following"))
        (should (= calls 0))
        (opencode-session--handle-event
         'session.text.delta (opencode-sessions-test--text-delta " prose.\n"))
        (should (= calls 1))
        (let ((output (buffer-substring-no-properties
                       (point-min) (process-mark process))))
          (should (string-match-p "| +7 +|" output))
          (should (string-suffix-p "Following prose.\n" output)))
        (should (equal (buffer-substring-no-properties
                        (process-mark process) (point-max))
                       "pending input"))
        (should (= (point) (point-max)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "opencode-sessions.el")
          (should (button-at (1- (point))))
          (search-forward "7 |")
          (should (eq (get-text-property (- (point) 4) 'field) 'output)))
        ;; A fenced table is code and must never be aligned.
        (opencode-session--handle-event
         'session.text.delta
         (opencode-sessions-test--text-delta
          "\n```markdown\n| Code | Value |\n| --- | --- |\n| x | y |\n```\n\n"))
        (should (= calls 1))
        ;; A blank line completes a table before the text part ends.
        (opencode-session--handle-event
         'session.text.delta
         (opencode-sessions-test--text-delta "| Name | Value |\n| --- | --- |\n| x | y |\n\n"))
        (should (= calls 2))
        (opencode-session--handle-event
         'session.text.delta (opencode-sessions-test--text-delta "More prose.\n\n"))
        (should (= calls 2))
        ;; Flush a final table without a trailing newline on part completion.
        (opencode-session--handle-event
         'session.text.delta
         (opencode-sessions-test--text-delta "| Last | Value |\n| --- | --- |\n| x | y |"))
        (should (= calls 2))
        (opencode-session--handle-event
         'session.text.ended
         '((sessionID . "session") (assistantMessageID . "message") (ordinal . 0)
           (text . "")))
        (should (= calls 3))
        (should-not opencode--table-scan-marker)
        (should (eq (char-before (process-mark process)) ?|))
        (should (equal (buffer-substring-no-properties
                        (process-mark process) (point-max))
                       "pending input"))))))

(ert-deftest opencode-session-reasoning-leading-newline-test ()
  "A streamed reasoning part starting with a newline adds no blank line or margin."
  (with-opencode-session-test-buffer
    (opencode--output "request\n\n")
    (opencode-session--handle-event
     'session.reasoning.started
     '((sessionID . "session") (assistantMessageID . "message") (ordinal . 0)))
    (opencode-session--handle-event
     'session.reasoning.delta
     '((sessionID . "session") (assistantMessageID . "message") (ordinal . 0) (delta . "\n")))
    (opencode-session--handle-event
     'session.reasoning.delta
     '((sessionID . "session") (assistantMessageID . "message") (ordinal . 0) (delta . "Thinking")))
    (should (equal (buffer-substring-no-properties
                    (point-min) (process-mark process))
                   "request\n\nThinking"))
    (dolist (pos (number-sequence (point-min) (+ (point-min) 8)))
      (should-not (seq-some (lambda (ov) (overlay-get ov 'line-prefix))
                            (overlays-at pos))))
    (should (seq-some (lambda (ov) (overlay-get ov 'line-prefix))
                      (overlays-at (+ (point-min) 9))))))

(ert-deftest opencode-session-fence-at-process-mark-test ()
  "Parsing Markdown must not fail when output ends on a code fence line."
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode--output "hi\n\n```text")
    (syntax-propertize (point-max))
    (font-lock-ensure)
    (opencode--output "\ncode\n```")
    (insert "pending")
    (syntax-propertize (point-max))
    (font-lock-ensure)))

(ert-deftest opencode-session-table-alignment-context-test ()
  "Table alignment must not disturb the session buffer's comint keymap."
  (with-opencode-session-test-buffer
    (insert "| Name | Value |\n| --- | --- |\n| x | y |\n")
    (opencode--align-output-table (point-min) (point-max))
    (should (eq (key-binding (kbd "RET")) 'comint-send-input))))

(ert-deftest opencode-session-history-table-alignment-test ()
  "Align replayed assistant tables, preserving user requests and code fences."
  (with-opencode-session-test-buffer
    (let* ((table "| Name | Value |\n| --- | ---: |\n| x | y |")
           (fenced (concat "```markdown\n" table "\n```"))
           (messages
            `(((type . "user") (id . "u1") (text . ,table))
              ((type . "assistant") (id . "a1") (agent . "orchestrator")
               (model . ((id . "m") (providerID . "p")))
               (tokens . ((input . 0) (output . 0) (reasoning . 0)
                          (cache . ((read . 0) (write . 0)))))
               (content . (((type . "text") (text . ,table))
                           ((type . "text") (text . ,fenced))))))))
      ;; Capture the request separately to verify it reaches the user
      ;; renderer without going through the table alignment path.
      (cl-letf (((symbol-function 'opencode--replay-user-request)
                 (lambda (message)
                   (opencode--output (alist-get 'text message))
                   (opencode--output "\n\n"))))
        (opencode--replay-session-messages messages))
      (let ((output (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-prefix-p (concat table "\n\n") output))
        (should (string-match-p "| +x +| +y +|" output))
        (should (string-suffix-p (concat fenced "\n\n") output)))
      (should-not opencode--table-scan-marker)
      (should (= (process-mark process) (point-max))))))

(ert-deftest opencode-session-agent-sync-by-id-test ()
  "Treat JSON false/null as falsy, sync agents by id, and skip null models."
  (dolist (value '(nil :null :false 0))
    (should (opencode--json-falsy value)))
  (should-not (opencode--json-falsy t))
  (let* ((plan '((id . "plan") (name . "Plan") (mode . "primary")
                 (hidden . :false) (model . :null)))
         (build '((id . "build") (name . "Build") (mode . "primary")
                  (hidden . :false) (model . :null)))
         (opencode-api-url "http://opencode.test")
         requests)
    (cl-letf (((symbol-function 'plz)
               (lambda (_method url &rest args)
                 (push (cons url (plist-get args :body)) requests)
                 (funcall (plist-get args :then) nil))))
      (with-temp-buffer
        (setq opencode-session-id "ses_test"
              opencode-session-agents (list (copy-tree plan) (copy-tree build))
              opencode-session-agent (car opencode-session-agents))
        (opencode-cycle-session-agent)
        (should (equal "build" (alist-get 'id opencode-session-agent)))
        (opencode-session--sync-agent-and-model #'ignore)
        (should (equal requests
                       '(("http://opencode.test/api/session/ses_test/agent"
                          . "{\"agent\":\"build\"}"))))
        (opencode-session--sync-agent-from-message
         '((agent . "plan") (model . ((id . "m") (providerID . "p")))))
        (should (equal "plan" (alist-get 'id opencode-session-agent)))
        (should (equal "plan" opencode-session--synced-agent))))))

(provide 'opencode-sessions-test)
;;; opencode-sessions-test.el ends here
