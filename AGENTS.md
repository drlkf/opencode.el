# AGENTS.md - opencode.el

Emacs frontend for OpenCode LLM coding assistant server.

# Tool use

Interact with the user's emacs process to perform introspection, find
information about functions, values of variables etc. Use it to run and verify
that code you write is working correctly.

## Build & Verification Commands

After you are finished editing any .el file, run `./check.sh` and fix any warnings.

### Naming Conventions

| Type | Pattern | Example |
|------|---------|---------|
| Public function | `opencode-<name>` | `opencode-process-events` |
| Private function | `opencode--<name>` | `opencode--log-event` |
| Public variable | `opencode-<name>` | `opencode-host` |
| Private variable | `opencode--<name>` | `opencode--plz-event-request` |
| Buffer-local var | `opencode-<name>` with `defvar-local` | `opencode-session-id` |

### Customization Variables

Use `defcustom` for user-configurable values:

```elisp
(defcustom opencode-host "localhost"
  "Hostname for the opencode server."
  :type 'string
  :group 'opencode)
```

### Docstrings

- First line: imperative sentence, fits in 80 chars
- Mention all arguments.
- Arguments in CAPS: `"Send STRING as input using PROC."`

### Control Flow Patterns

**Use `let-alist` for JSON/alist destructuring:**
```elisp
(let-alist properties
  (notifications-notify
   :title .title
   :body .message))
```

**Use `pcase` for pattern matching:**
```elisp
(pcase .type
  ("text" (insert .text))
  ("reasoning" (opencode--insert-reasoning-block text)))
```

### Async HTTP Pattern

This codebase uses macro-generated async API calls:

```elisp
;; The macro opencode-api-session expands to async plz call
(opencode-api-session (session-id)
    result-var
  ;; This body runs in callback with result-var bound
  (do-something-with result-var))
```

### Error Handling

- Use `error` for fatal conditions
- Log warnings to dedicated buffer: `*opencode-event-log*`
  use `(opencode--log-event "WARNING TYPE" "msg")`

---

## Architecture Notes

### File Responsibilities

| File                   | Role                                               |
|------------------------|----------------------------------------------------|
| `opencode.el`          | Entry point, SSE event handling, notifications     |
| `opencode-api.el`      | HTTP API macro generator, endpoint definitions     |
| `opencode-sessions.el` | Session UI, comint integration, markdown           |
| `opencode-common.el`   | Utility functions and variable needed project-wide |
