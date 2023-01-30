declare module "DependsOnRealModule" {
  import type {RealType} from "RealModule";
  export type T = RealType;
}
