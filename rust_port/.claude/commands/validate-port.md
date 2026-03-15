---
description: Validate that a Rust port follows all OCaml-to-Rust porting rules
---


You should:
1. Identify whether {{prompt}} is an OCaml or Rust file
2. Find the corresponding Rust file (if given OCaml) or OCaml files (if given Rust)
3. Decide how many functions/structs/types/methods/etc you need to check. Chunk them into many manageable sections, and then launch many
`rust-port-validator` agent in parallel to do the job of systematic validation against all porting rules (structure, order,
   API signatures, whether it's faithful line-by-line port)
4. Once the step above is done, fix issues in order from easiest to hardest (wrong order → API mismatches →
   structural deviations)
5. Provide a final summary of all issues found and fixed
