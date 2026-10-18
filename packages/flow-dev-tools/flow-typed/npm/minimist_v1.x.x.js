// flow-typed signature: d48da8db828529253fc20b80747846ea
// flow-typed version: c6154227d1/minimist_v1.x.x/flow_>=v0.104.x

declare module 'minimist' {
  declare type minimistOptions = {
    string?: string | ReadonlyArray<string>,
    boolean?: boolean | string | ReadonlyArray<string>,
    alias?: { readonly [arg: string]: string | ReadonlyArray<string>, ... },
    default?: { [arg: string]: unknown, ... },
    stopEarly?: boolean,
    '--'?: boolean,
    unknown?: (param: string) => boolean,
    ...
  };

  declare type minimistOutput = {
    [flag: string]: unknown,
    _: Array<string | number>,
    '--'?: Array<string>,
    ...
  };

  declare module.exports: (argv: ReadonlyArray<string>, opts?: minimistOptions) => minimistOutput;
}
