;;; opencode-sessions-test.el --- Session rendering tests  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'opencode-sessions)

(ert-deftest opencode-read-tool-file-location-button-test ()
  "Format read offsets as working file-and-line buttons."
  (let* ((file-directory (file-name-directory
                          (locate-library "opencode-sessions")))
         (default-directory file-directory)
         (formatted (opencode--format-tool-call
                     "read" '((filePath . "opencode-sessions.el")
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
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (let ((buffer (generate-new-buffer "opencode-markdown-test")))
    (unwind-protect
        (with-current-buffer buffer
          (setq default-directory (file-name-directory (locate-library "opencode-sessions")))
          (opencode-session-mode)
          (font-lock-mode 1)
          (let ((process (start-process "opencode-markdown-test" buffer nil)))
            (unwind-protect
                (progn
                  (opencode--output "# Heading\n\nSee `opencode-sessions")
                  (opencode--output ".el` and *emphasis*.\n\n")
                  (opencode--insert-tool-block
                   "edit" '((filePath . "opencode-sessions.el")
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
                    (should (memq 'markdown-ts-heading-1
                                  (get-text-property (1- (point)) 'face)))
                    (goto-char (point-min))
                    (search-forward "opencode-sessions.el")
                    (let ((button (button-at (1- (point)))))
                      (should button)
                      (should (equal (car (button-get button 'button-data))
                                     (expand-file-name "opencode-sessions.el"))))
                    (goto-char (point-min))
                    (search-forward "-old")
                    (should (memq 'diff-removed
                                  (get-text-property (1- (point)) 'face)))
                    (should (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                                     (overlays-at (1- (point)))))
                    (search-forward "```")
                    (should-not (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                                         (overlays-at (- (point) 2))))
                    (search-forward "**Testing button behavior**")
                    (should (memq 'markdown-ts-bold
                                  (get-text-property (- (point) 27) 'face)))
                    (should (get-char-property (- (point) 27) 'invisible))
                    (search-forward "(+ 1 2)")
                    (should (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                                     (overlays-at (1- (point)))))
                    (search-forward "```")
                    (should-not (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                                         (overlays-at (- (point) 2))))
                    (goto-char (point-min))
                    (search-forward "> ")
                    (should (get-char-property (- (point) 2) 'invisible))))
              (delete-process process))))
      (kill-buffer buffer))))

(ert-deftest opencode-session-diff-refinement-test ()
  "Refine entire synthetic hunks without touching input or earlier output."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (with-temp-buffer
    (opencode-session-mode)
    (let ((process (start-process "opencode-refinement-test" (current-buffer) nil))
          (diff-refine 'font-lock))
      (unwind-protect
          (progn
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
                (should (get-char-property (- (point) 11) 'invisible)))))
        (delete-process process)))))

(ert-deftest opencode-session-yank-code-block-test ()
  "Copy only code from a fenced block, including embedded fences."
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (with-temp-buffer
    (opencode-session-mode)
    (insert (opencode--fenced-block "```\ntext\n" "text") "\n")
    (goto-char (point-min))
    (search-forward "text" nil nil 2)
    (let ((node (treesit-node-at (point) 'markdown)))
      (while (and node (not (equal (treesit-node-type node) "fenced_code_block")))
        (setq node (treesit-node-parent node)))
      (should node))
    (opencode-yank-code-block)
    (should (equal (current-kill 0 t) "```\ntext\n"))))

(provide 'opencode-sessions-test)
;;; opencode-sessions-test.el ends here
