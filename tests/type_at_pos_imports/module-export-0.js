// @flow

declare module a {
  declare export class A {}
}

declare var n: $Exports<'a'>;
export const m = n;
