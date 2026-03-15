---
name: reorder-declarations
description: Reorder Rust file declarations to match OCaml source order. Use when porting OCaml to Rust and declarations are out of order, or when asked to fix ordering to match OCaml.
---

# Reorder Declarations Skill

Reorder declarations in a Rust port file to exactly match the OCaml source file order.

## When to Use

- After porting OCaml to Rust when declaration order doesn't match
- When asked to "fix order" or "reorder" a Rust port to match OCaml
- When validating a port reveals ordering issues
- When porting rules specify matching OCaml declaration order

## Workflow

The entire workflow **MUST** be run in a subagent. You **MUST** follow this sequence of steps sequentially and exactly. Don't come up with your own smart way. Trust me, you will always screw up when you do that. **NO CREATIVITY ALLOWED**.

### Step 1: List all top-level names in Rust

Read the Rust .rs file and manually list every declaration `(struct, enum, type, mod, fn)` with their names. `fn` within a toplevel `impl` should be considered as toplevel as well. You must include every single toplevel item, regardless of their visibility. You must not list non-toplevel items, such as members of an enum. Don't filter anything. Write to rust_decls.md in the **current project directory**. At this stage, rust_decls.md must be a markdown file of list **ONLY**. **ONLY LIST. ONE GIANT LIST ONLY**.

### Step 2: Associate each Rust declaration with an OCaml one

Read the OCaml .ml file. For each Rust declaration in rust_decls.md, find which OCaml declaration it corresponds to by matching names (considering naming conventions). You must inspect the result carefully. Having exact match doesn't mean that you found the declaration in the ocaml code, due to naming convention differences. Write down the mapping with the OCaml line number inside in rust_decls.md. You are only allowed to add the ocaml line number to the left of the name. e.g.

```diff
- * struct Foo
+ * (1337) struct Foo
- * enum Bar
+ * (42) enum Bar
```

**DO NOT SKIP ITEMS. DO NOT STOP. Work through every single item until every single item has a line number.** When you are unsure, ask me. At this stage, rust_decls.md must be a markdown file of list **ONLY**.  **ONLY LIST. ONE GIANT LIST ONLY**.

### Step 3: Sort the list into OCaml order

Sort your Rust declarations in rust_decls.md based on the line number you found in the last step. Create a sorted list showing the desired final order. Overwrite rust_decls.md with the sorted list. At this stage, rust_decls.md must be a markdown file of list **ONLY**.  **ONLY LIST. ONE GIANT LIST ONLY**.

### Step 4: Turn the list into a checklist

Based on the sorted order, create a TODO list where each item is:
"- [ ] <Rust declaration name>"

The line number should be removed in this step.

Use TodoWrite to create this checklist. The checklist must be put into the current project directory as order.md. At this stage, order.md must be a markdown file of list **ONLY**. **ONLY** list. No title. No comments. No judgement.

### Step 5: Move things in checklist order

For each TODO item in order.md:
1. Read the rust file to find the declaration
2. Copy the COMPLETE declaration (with all attributes, comments, braces)
3. Delete it from current location using Edit
4. Insert it right after the previous completed item using Edit
5. Mark TODO as completed
6. Move to next item

**DO NOT SKIP ITEMS. DO NOT STOP. Work through every single item.**

### Step 6: Verify build

After all moves complete:
```bash
buck2 build <target>
```

## Important Rules

- Read files directly to list declarations
- List ALL Rust declarations first
- THEN map to OCaml order
- Move ONE declaration at a time
- Always mark TODO done after each move
- Don't stop until all TODOs are checked off
