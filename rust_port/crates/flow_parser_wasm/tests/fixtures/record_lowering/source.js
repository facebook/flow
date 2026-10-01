record R {
  default: number,
  1: number = 1,
  value: string,
  method() {}
  static item: number = 2,
  static make() {}
}
const result = R {...base, child: S {value: 1}};
