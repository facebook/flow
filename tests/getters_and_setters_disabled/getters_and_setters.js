/**
 * @flow
 */

var f = {
  get a() { return 4; },
  set b(x: number) { this.c = b; },
  c: 10,
  get ['d']() { return 'foo'; },
  set ['d'](x: number) {},
};
