class R {
  constructor({default: $$gen$r0, 1: $$gen$r1 = 1, value}) {
    this.default = $$gen$r0;
    this[1] = $$gen$r1;
    this.value = value;
  }
  method() {}
  static item = 2;
  static make() {}
}
const result = new R({ ...base, child: new S({ value: 1 }) });
