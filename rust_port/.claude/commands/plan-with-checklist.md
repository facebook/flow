# Planning Instructions

Create a comprehensive plan for the requested task following these requirements:

## Plan Structure Requirements

1. **Create plan files in BOTH locations:**
   - System plan file (the one specified in plan mode)
   - Working directory: `./plan.md` (current directory)

2. **Phase-based structure with explicit checkpoints:**
   - Break work into logical phases
   - Each phase ends with: `**CHECKPOINT: User reviews [phase] before proceeding**`
   - STOP at each checkpoint and wait for user approval

3. **Detailed checklist format:**
   ```markdown
   ### Phase N: [Phase Name]
   - [ ] Specific task 1
   - [ ] Specific task 2
   - [ ] Verify compiles/tests pass

   **CHECKPOINT: User reviews [what] before proceeding**
   ```

4. **Checklist rules:**
   - Only mark items `[x]` when ACTUALLY completed
   - TODO stubs don't count as complete (unless explicitly allowed)
   - Must verify completion before checking off
   - Update checklist in real-time as you work

5. **Planning process:**
   - First, ask at least 10 clarification questions about:
     * Architecture decisions
     * Dependencies and build system
     * Testing approach
     * Coding standards
     * Scope and boundaries
     * Error handling strategy
     * Performance requirements
     * Integration points
     * Documentation needs
     * Deployment/rollback strategy
   - Create topological ordering if multiple files involved
   - List all files to be created/modified

6. **Use TodoWrite for active tracking:**
   - Create todo items for current phase
   - Update status as you progress
   - Clear completed items before next phase

7. **Verification at each phase:**
   - Code must compile (if applicable)
   - Tests must pass (if applicable)
   - Documentation updated (if applicable)

## Example Plan Structure

```markdown
# Plan: [Task Description]

## Overview
[Brief description of the task]

## Progress Checklist

### Phase 1: [Setup/Foundation]
- [ ] Create directory structure
- [ ] Create build configuration
- [ ] Verify builds
**CHECKPOINT: User reviews structure before proceeding**

### Phase 2: [Core Implementation]
- [ ] Implement feature A
- [ ] Implement feature B
- [ ] Add tests
- [ ] Verify all tests pass
**CHECKPOINT: User reviews implementation before proceeding**

### Phase 3: [Integration]
- [ ] Integrate with system X
- [ ] Update documentation
- [ ] Run integration tests
**CHECKPOINT: User reviews integration before proceeding**

## Technical Decisions
| Question | Answer |
|----------|--------|
| Architecture | ... |
| Dependencies | ... |
| Testing | ... |

## File Manifest
- `path/to/file1.rs` - [description]
- `path/to/file2.rs` - [description]
```

## Execution Rules

1. **Stop at every checkpoint** - Don't proceed past checkpoints without explicit user approval
2. **Update both plan files** - Keep both system and working directory plans in sync
3. **Verify before marking complete** - Run builds/tests before checking items off
4. **No assumptions** - Ask for clarification when unclear
5. **Incremental progress** - Complete one phase fully before starting next

When user says "continue" or "good continue" at a checkpoint, proceed to next phase.

---

Now apply these planning instructions to: $ARGUMENTS
