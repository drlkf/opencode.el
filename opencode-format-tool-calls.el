;;; opencode-format-tool-calls.el --- Code for managing opencode sessions  -*- lexical-binding: t; -*-

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

;; Code for managing formatting opencode tool calls for display

;;; Code:

(require 'diff)
(require 'diff-mode)
(require 'opencode-common)

(defun opencode--format-tool-call (tool input)
  "Format TOOL call with INPUT arguments for display."
  (let-alist input
    (when .filePath
      (setf .filePath (opencode--relative-path-for-display .filePath)))
    (pcase tool
      ("edit"
       (concat "edit " .filePath ":\n"
               (opencode--format-edit-diff .oldString .newString)
               "\n"))
      ("apply_patch"
       (concat "apply_patch:\n"
               (opencode--format-apply-patch .patchText)
               "\n"))
      ("read"
       (if (and .offset .limit)
           (format "read %s [offset=%d, limit=%d]\n\n"
                   .filePath .offset .limit)
         (format "read %s\n\n" .filePath)))
      ("grep"
       (concat
        (format "grep \"%s\"" .pattern)
        (when (or .include .path)
          (format " in %s" (or .include .path)))
        "\n\n"))
      ("bash"
       (concat (when .description
                 (format "# %s\n" .description))
               (format "$ %s\n\n" .command)))
      ("websearch"
       (format "websearch \"%s\"\n\n" .query))
      ("call_omo_agent"
       (format "call_omo_agent: %s\n\n%s\n\n" .description .prompt))
      ("glob"
       (if .path
           (format "glob \"%s\" in %s\n\n"
                   .pattern
                   (opencode--relative-path-for-display .path))
         (format "glob \"%s\"\n\n" .pattern)))
      ("todowrite"
       (concat (opencode--render-todos .todos) "\n\n"))
      ("question"
       (concat (opencode--format-questions (alist-get 'questions input)) "\n\n"))
      ((and "task" (guard (string= "explore" .subagent_type)))
       (format "🔍 Explore: %s\n\n" .description))
      (_ (if (= 1 (length input))
             (format "%s %s\n\n" tool (cdar input))
           ;; Multiple arguments: tool-name, then arg-name: value per line
           (concat tool " ["
                   (mapconcat (lambda (pair)
                                (format "%s=%s" (car pair) (cdr pair)))
                              input
                              ", ")
                   "]\n\n"))))))

(defun opencode--render-todos (todos)
  "Render TODOS as markdown todo list."
  (opencode--render-markdown
   (mapconcat
    (lambda (todo)
      (let-alist todo
        (format "%s %s"
                (pcase .status
                  ("pending" "📌")
                  ("in_progress" "▶")
                  ("completed" "✅")
                  ("cancelled" "❌")
                  (_ " "))
                .content)))
    todos
    "\n")))

(defun opencode--fontify-diff-string (diff-string)
  "Return DIFF-STRING with `diff-mode' faces applied."
  (with-temp-buffer
    (insert diff-string)
    (delay-mode-hooks (diff-mode))
    (font-lock-ensure)
    (buffer-string)))

(defun opencode--format-edit-diff (old-string new-string)
  "Generate diff output comparing OLD-STRING to NEW-STRING."
  (with-temp-buffer
    (let ((old-buf (current-buffer)))
      (insert old-string)
      (insert "\n")
      (with-temp-buffer
        (let ((new-buf (current-buffer)))
          (insert new-string)
          (insert "\n")
          (with-temp-buffer
            (let ((inhibit-read-only t))
              ;; Run diff synchronously into this temp buffer
              (diff-no-select old-buf new-buf nil t (current-buffer))
              ;; Delete first 3 lines (diff command, ---, +++)
              (goto-char (point-min))
              (forward-line 3)
              (delete-region (point-min) (point))
              ;; Delete last 2 lines (diff finished timestamp)
              (goto-char (point-max))
              (forward-line -2)
              (delete-region (point) (point-max))
              ;; Return the diff content
              (opencode--fontify-diff-string (buffer-string)))))))))

(defun opencode--format-apply-patch (patch-text)
  "Return PATCH-TEXT formatted for `apply_patch' display."
  (opencode--fontify-diff-string
   (with-temp-buffer
     (insert patch-text)
     (goto-char (point-min))
     (while (re-search-forward "^\\*\\*\\* \\(?:Begin\\|End\\) Patch\\n?" nil t)
       (replace-match "" t t))
     (goto-char (point-min))
      (while (not (eobp))
        (cond
        ((looking-at "\\*\\*\\* \\(Add File\\|Update File\\|Delete File\\): \\(.*\\)$")
         (let ((beg (match-beginning 0))
               (end (match-end 0))
               (operation (match-string 1))
               (file (match-string 2)))
           (delete-region beg end)
           (goto-char beg)
           (insert (format "*** %s: %s"
                           operation
                           (opencode--relative-path-for-display file)))))
        ((looking-at "\\*\\*\\* Move to: \\(.*\\)$")
         (let ((beg (match-beginning 0))
               (end (match-end 0))
               (file (match-string 1)))
           (delete-region beg end)
           (goto-char beg)
           (insert (format "*** Move to: %s"
                           (opencode--relative-path-for-display file)))))
        ((looking-at "@@\\(.*\\)$")
         (let ((beg (match-beginning 0))
               (end (match-end 0))
               (suffix (match-string 1)))
           (unless (save-match-data
                     (string-match-p "\\` -[0-9,]+ \\+[0-9,]+ @@" suffix))
              (delete-region beg end)
              (goto-char beg)
              (insert (format "@@ -0 +0 @@%s" suffix))))))
        (forward-line 1))
      (goto-char (point-min))
      (skip-chars-forward "\n")
      (delete-region (point-min) (point))
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (delete-region (point) (point-max))
      (buffer-string))))

(provide 'opencode-format-tool-calls)
;;; opencode-format-tool-calls.el ends here
