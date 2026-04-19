;;; opencode-common.el --- Common code shared through the package  -*- lexical-binding: t; -*-

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

;; Common code shared through the package

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'map)
(require 'markdown-mode)
(require 'project)
(require 'savehist)

(defvar opencode-last-model '((providerID . "opencode")
                              (modelID . "big-pickle"))
  "Last model used.")
(add-to-list 'savehist-additional-variables 'opencode-last-model)

(defvar opencode-agents nil
  "List of available agents excluding hidden agents.")

(defvar opencode-slash-commands nil
  "List of available slash commands.")

(defvar opencode-alerted-sessions nil
  "List of unvisited idle sessions.")

(defvar opencode-last-session-buffer nil
  "Last active opencode session buffer.")

(defvar opencode-providers nil
  "List of available providers and models.")

(defvar opencode-part-type (make-hash-table :test 'equal)
  "Mapping of part id's to their type.")

(defvar-local opencode--extra-parts nil
  "An list containing extra parts to send with the next input.")

(defvar opencode--event-subscriptions nil
  "An alist mapping: SSE event process to it's directory.")

(defvar opencode--session-control-buffers nil
  "An alist mapping of projectIDs to session control buffers for that project.")

(defcustom opencode-toast-function 'opencode--default-toast-show
  "Function to use to show toast notifications.
It receives a single argument, PROPERTIES, which is an alist with string keys
for title, message, variant (error/warning/info/success), and duration"
  :type 'function
  :group 'opencode)

(defcustom opencode-server-username "opencode"
  "Username to connect to opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-server-password nil
  "Password to connect to opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-show-tool-output nil
  "Show output of tool calls."
  :type 'boolean
  :group 'opencode)

(defun opencode--time-ago (opencode-object type)
  "Return .time.TYPE value from OPENCODE-OBJECT, as seconds ago.
Returns nil if the timestamp is not present."
  (when-let ((timestamp (map-nested-elt opencode-object `(time ,type))))
    (- (float-time) (/ timestamp 1000))))

(defun opencode--format-time-ago (seconds)
  "Format SECONDS as a human-readable duration string.
Returns \"-\" if SECONDS is nil."
  (if seconds
      (seconds-to-string seconds)
    "-"))

(defun opencode--json-falsy (value)
  "Return t if VALUE is null, :null, or 0."
  (cl-case value
    ((nil :null 0) t)
    (t nil)))

(defun opencode--truncate-at-max-lines (max-lines)
  "Delete the first half of the buffer if we've reached MAX-LINES."
  (when (> (line-number-at-pos) max-lines)
    (goto-char (point-max))
    (forward-line (- (/ max-lines 2)))
    (delete-region (point-min) (point))))

(defun opencode--annotated-completion (prompt candidates)
  "Simplified and formatted completing read with PROMPT.
CANDIDATES is a list of lists, where the first element of each list is the
string to show, the second is the value to return, and the third is the
annotation to show."
  (let* ((max-length (seq-max
                      (mapcar (lambda (candidate)
                                (length (car candidate)))
                              candidates)))
         (candidate-results (make-hash-table :test 'equal))
         (candidates (cl-loop for (name return-value maybe-annotation) in candidates
                              for annotation = (or maybe-annotation "")
                              for candidate = (concat name
                                                      (propertize annotation 'invisible t))
                              do (puthash candidate return-value candidate-results)
                              collect (propertize candidate
                                                  'opencode-annotation
                                                  (concat (make-string (+ 5 (- max-length
                                                                               (length name)))
                                                                       ?\s)
                                                          annotation))))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (candidate)
               (get-text-property 0 'opencode-annotation candidate)))))
    (gethash
     (completing-read
      prompt
      candidates
      nil t)
     candidate-results)))

(defun opencode--format-questions (questions)
  "Format QUESTIONS showing ❓ prefix for each question."
  (string-join
   (cl-loop for q being the elements of questions
            for question-text = (alist-get 'question q)
            collect (concat "❓ " question-text "\n"))))

(defun opencode--relative-path-for-display (file)
  "Return FILE relative to project root when possible."
  (when file
    (let* ((project (project-current))
           (root (if project
                     (project-root project)
                   default-directory)))
      (if (and (file-name-absolute-p file)
               root
               (file-in-directory-p file root))
          (file-relative-name file root)
        file))))

(defun opencode--resolve-file-reference (file)
  "Return absolute path for FILE, or nil if no file exists."
  (let ((project (project-current)))
    (seq-find (lambda (candidate)
                (and candidate
                     (file-exists-p candidate)
                     (not (file-directory-p candidate))))
              (delete-dups
               (delq nil
                     (list (and (file-name-absolute-p file) file)
                           (when project
                             (expand-file-name file (project-root project)))
                           (expand-file-name file default-directory)))))))

(defun opencode--visit-file-location (location)
  "Open LOCATION in another window."
  (let ((file (car location))
        (line (cdr location)))
    (find-file-other-window file)
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))

(defun opencode--buttonize-file-references (string)
  "Return STRING with existing file references turned into buttons."
  (let ((start 0)
        (regexp "\\(`?\\)\\([[:alnum:]._~/][[:alnum:]@%_./~+-]*\\):\\([0-9]+\\)\\1")
        parts)
    (while (string-match regexp string start)
      (let* ((match-beg (match-beginning 0))
             (match-end (match-end 0))
             (file (match-string 2 string))
             (line (string-to-number (match-string 3 string)))
             (path (opencode--resolve-file-reference file)))
        (push (substring string start match-beg) parts)
        (push (if path
                  (make-text-button
                   (substring string match-beg match-end)
                   nil
                   'action #'opencode--visit-file-location
                   'button-data (cons path line)
                   'follow-link t
                   'help-echo (format "Open %s:%d in another window" file line)
                   'mouse-face 'highlight)
                (substring string match-beg match-end))
              parts)
        (setq start match-end)))
    (push (substring string start) parts)
    (apply #'concat (nreverse parts))))

(defun opencode--render-markdown (string)
  "Render STRING in `gfm-view-mode'."
  (with-temp-buffer
    (insert string)
    (delay-mode-hooks (gfm-view-mode))
    (font-lock-ensure)
    (buffer-string)))

(provide 'opencode-common)
;;; opencode-common.el ends here
