# Improve Skillbook

Launch the Skillbook Manager agent to analyze the current porting session and
update the skillbook.

## Usage

This command combines reflection and skillbook updates into a single workflow:

1. **Analyze** the current conversation for what worked/failed
2. **Extract** atomic, high-value learnings
3. **Propose** skillbook updates (ADD/UPDATE/REMOVE)
4. **Validate** proposals (atomicity, duplicates, low-level filtering)
5. **Apply** approved changes
6. **Report** summary

## When to Use

Run this command **after completing a porting session**, typically after:

- Finishing the Rust port
- Running `/validate-port` and fixing any errors
- Running `/check-equivalence` and addressing issues

## Your Task

Launch the skillbook-manager agent and tell it to:

1. **First, reflect on the current conversation:**
   - Review the entire conversation history
   - Identify what porting strategies worked (which skills were cited and
     helpful)
   - Identify what failed (validation errors, build errors, manual fixes needed)
   - Extract atomic, non-trivial learnings

2. **Then, propose skillbook updates:**
   - NEW skills to add (with atomicity validation)
   - EXISTING skills to update (helpful/harmful counts)
   - HARMFUL skills to remove
   - Filter out low-level/obvious patterns

3. **Finally, apply approved changes:**
   - Validate each proposal
   - Ask user for borderline cases
   - Update `.claude/data/rust_port_skillbook.md`
   - Report detailed summary

## Quality Reminders

The agent should reject:

- Trivial type mappings (e.g., "option → Option")
- Basic functional patterns (e.g., "map → map")
- Duplicate skills (>70% overlap)
- Compound concepts (atomicity < 0.7)

The agent should accept:

- Ownership decisions
- Concurrency patterns
- Subtle correctness issues
- Performance insights
- Architectural patterns
