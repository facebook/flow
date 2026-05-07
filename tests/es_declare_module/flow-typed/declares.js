declare module "CJS_Named" {
  declare const num1: number;
  declare const str1: string;
}

declare module "CJS_Clobbered" {
  declare const num2: number;
  declare type numType = number;
  declare module.exports: {
    numExport: number,
    ...
  };
}

declare module "ES" {
  declare const strHidden: string;
  declare export {strHidden as str3};
  declare export const num3: number;
  declare export class C {}
  declare export type T = number;
  declare const exports: number;
}

declare module "re-export" {
  import type {Foo} from 'to-import';
}
