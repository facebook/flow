---
description: Check semantic equivalence between OCaml and Rust port
---

Verify that {{prompt}} is a faithful port with equivalent semantics.

You should:
1. Identify whether {{prompt}} is an OCaml or Rust file
2. Find the corresponding Rust file (if given OCaml) or OCaml files (if given Rust)
3. Decide how many functions/structs/types/methods/etc you need to check. Chunk them into many manageable sections, and then launch many
`rust-port-semantic-equivalence-checker` agent in parallel to do the job.
4. Distinguish between expected language differences and critical discrepancies
5. Generate a detailed markdown report with:
   - Executive summary of port faithfulness
   - Section-by-section analysis
   - Expected differences (non-critical)
   - Critical differences with severity and recommendations
   - Missing elements
   - Statistics and conclusion
