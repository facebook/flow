declare module 'toplevel-declared-module' {
  declare module 'nested-declare-module1' {}
}

{
  declare module 'nested-declare-module2' {}
}

if (true) {
  declare module 'nested-declare-module3' {}
}
