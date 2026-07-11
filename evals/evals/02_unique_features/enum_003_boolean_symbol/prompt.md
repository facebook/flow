Write Flow code using two different enum representation types.

1. Define a boolean enum `Toggle` with members `On` (true) and `Off` (false).

2. Define a symbol enum `Priority` with members `Low`, `Medium`, `High`, `Critical`.

Write:
- `applyToggle(current: number, toggle: Toggle): number` — if `On`, double the value; if `Off`, return 0
- `priorityWeight(p: Priority): number` — return 1 for `Low`, 2 for `Medium`, 4 for `High`, 8 for `Critical`
- `shouldAlert(p: Priority, toggle: Toggle): boolean` — return `true` only when the toggle is `On` and the priority is `High` or `Critical`
