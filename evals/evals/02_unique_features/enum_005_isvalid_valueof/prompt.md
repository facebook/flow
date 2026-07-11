Write Flow code for a color theme validator.

Define a number enum `Theme` with members `Light`, `Dark`, `HighContrast`, and `Solarized`.

Write:
- `validateThemes(inputs: Array<number>): Array<number>` — filter the input array, keeping only numbers that correspond to valid `Theme` members. Return the valid numbers.
- `serializeTheme`: an arrow function `(?Theme) => number | void` — if the theme is present, return its number representation; otherwise return `undefined`. Write it as a concise arrow with an expression body (no braces).
- `buildConfig(themes: Array<Theme>): Array<{name: string, value: number}>` — for each theme, produce an object with `name` set to the member name and `value` set to its number representation
