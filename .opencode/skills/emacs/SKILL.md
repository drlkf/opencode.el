---
name: emacs
description: Interact with the user's emacs process when developing in elisp. Use introspection to browse documentation and available functions, run and verify the result of code you write, etc.
---

NEVER use the `emacs` command in batch mode, ALWAYS use `emacsclient` for
evaluating elisp.

# How to run code in emacs

Use the bash tool to run:
```sh
emacsclient --eval<<EOF
{{ELISP CODE HERE}}
EOF
```

# Potentially useful elisp code

* get docstring for function
`(documentation 'function-name)`
