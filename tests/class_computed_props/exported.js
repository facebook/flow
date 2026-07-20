const s = 'foo' as const;
const m = 'go' as const;
const o = { k: 'ext' } as const;

export class E {
  [s]: 1 = 1;
  [m](): number { return 1; }
  [o.k]: number = 2;
  static [s]: string = 'x';
}
