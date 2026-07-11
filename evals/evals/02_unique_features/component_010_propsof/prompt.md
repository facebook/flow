Write Flow code in `main.js` for a component configuration system.

`Badge` takes `text: string`, `color: string = "gray"`, and `size: 'sm' | 'md' | 'lg' = 'md'`. It renders a `<span>` with the text. Set the span's inline `color` style to the `color` prop, and set `fontSize` to `"12px"` for `sm`, `"16px"` for `md`, or `"24px"` for `lg`.

Write a function `createBadgePresets` that takes a read-only array of badge configurations. Each configuration is the same shape as the props object you'd pass to `Badge` via JSX. Derive this type from `Badge` itself rather than re-declaring the prop fields by hand.

The function returns a `ReadonlyArray<React.Node>` by mapping each configuration to a `<Badge>` element, spreading the config as props.

Write an `App` component that calls `createBadgePresets` with at least 3 preset configurations (varying text, color, and size) and renders them in a `<div>`.

The code must pass `flow check` with zero errors.
