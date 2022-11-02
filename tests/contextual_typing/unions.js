//@flow

{
    const x: { ['A' | 'B' ]: (number) => number } = { [42]: (v) => v };
}
{
    const x: { ['A']: (number) => number } = { [42]: (v) => v };
}
{
    const x: { ... } | { ... } = { a: (v) => v };
}
