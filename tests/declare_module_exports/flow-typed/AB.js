declare module "A" {
  import typeof BClass from "B";

  declare export type AT = number;
}

declare module "B" {
  import type {AT} from "A";
  declare export type BT = AT;

  declare class Def { thisIsBDefault: true; }
  declare export default typeof Def;
}
