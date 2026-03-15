---
description: Clean up comments in Rust port to match OCaml source exactly
---

Use the rust-port-comment-cleanup agent to validate and fix all comments in {{prompt}}.

The agent should:
1. Identify whether {{prompt}} is an OCaml or Rust file
2. Find the corresponding files (Rust ↔ OCaml .ml and .mli)
3. Systematically verify ALL comments (regular and doc) against OCaml source
4. Remove extraneous comments not in OCaml (Phase 1)
5. Add missing comments from OCaml (Phase 2)
6. Wait 5 seconds for diagnostics after each edit
7. Provide final summary of all changes made
