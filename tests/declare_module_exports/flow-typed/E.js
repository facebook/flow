declare module "E" {
  import BClass from "B";
  declare module.exports: typeof BClass;
}
