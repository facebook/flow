interface Base { extent: number }
export default interface GeometryScaling<T = number> extends Base {
  value: T;
}
export type Local = GeometryScaling<string>;
