Write a Flow React component `AccessibleButton` in `main.js`.

Props:
- `label: string` (required) — exposed to callers as the prop `aria-label`
- `description: string` (optional) — exposed to callers as the prop `aria-description`, defaults to an empty string
- `role: string` (optional) — exposed to callers as the prop `data-role`, defaults to `"button"`
- `onClick: () => void` (required)
- `children: React.Node` (required)

The component renders a `<button>` element. Set the `aria-label` attribute to `label`, and set `aria-description` to `description`. Add a `data-testid` attribute whose value is `"btn-"` followed by the `role` value. Pass `onClick` and `children` through to the `<button>`.

The code must pass `flow check` with zero errors.
