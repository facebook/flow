Implement the `Card` component and the `renderWithOverrides` function used by the `App` component already in `main.js`.

`Card` takes a `title: string`, a `body: React.Node`, and a `variant` that is either `'default'` or `'highlight'` and defaults to `'default'`. It renders a `<section>` whose class is `card-highlight` when highlighted and `card` otherwise, with the `title` in an `<h3>` and the `body` below it.

`renderWithOverrides` takes two arguments: a `base` config object holding the props you would pass to `Card`, and an `overrides` object holding any subset of those same props. It returns a `Card` element created by spreading `base` and then `overrides`. Derive both parameter types from the `Card` component's type rather than re-declaring the prop fields by hand, so that any prop with a default value may be omitted from `base`.
