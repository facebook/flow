Write Flow code in `main.js` for a plugin-based panel system.

Define a type `PanelPlugin` that describes the type of a component accepting props `title: string` and `content: string`.

Write a function `renderPlugins` that takes `plugins: ReadonlyArray<PanelPlugin>` and `data: ReadonlyArray<{title: string, content: string}>`. It returns `React.Node`. For each entry in `data`, it picks the plugin at the corresponding index (wrapping around with modulo if there are fewer plugins than data entries), and renders that plugin component with the entry's `title` and `content`.

Write two concrete components that satisfy `PanelPlugin`:
- `CardPanel` takes `title: string` and `content: string`. It renders a `<div>` containing an `<h3>` with the title and a `<p>` with the content.
- `MinimalPanel` takes `title: string` and `content: string`. It renders a `<span>` containing the title, a colon and space, then the content.

Write an `App` component that calls `renderPlugins` with both panel components and at least 3 data entries, rendering the result inside a `<div>`.

The code must pass `flow check` with zero errors.
