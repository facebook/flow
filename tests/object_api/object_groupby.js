{
  declare const xs: Array<string>;

  const result = Object.groupBy(xs, (item: string) => item.length === 1 ? 'single' as const : 'other' as const);
  result as {['single' | 'other']: Array<string> | void}; // OK
}
