type Opt = ?{
  b: boolean,
  n: {
    nn: number,
  },
  s?: {
    ss: string,
  },
};

export type B = Opt?.['b'];
export type N = Opt?.['n']['nn'];
export type S = Opt?.['s']?.['ss'];
