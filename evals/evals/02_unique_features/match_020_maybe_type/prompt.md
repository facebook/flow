Write a Flow function `formatName(name: ?{first: string, last: string}): string` that uses a `match` expression to handle a maybe type.

- If the name is present, return `"{first} {last}"`
- If `null`, return `"Anonymous"`
- If `undefined`, return `"Unknown"`
