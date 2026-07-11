`main.js` defines a `UIElement` type: a disjoint union of `button`, `input`, and `divider` elements, where buttons and inputs each carry a nested `style.variant`.

Write a Flow function `getClassName(el: UIElement): string` that returns a CSS class name using a `match` expression with nested object patterns.

The function should match on both the `tag` and the nested `style.variant` to determine the class name, and extract the `label` for buttons to include in the result.
