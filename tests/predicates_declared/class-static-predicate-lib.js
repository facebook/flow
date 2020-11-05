// @flow

declare export class Seq {
  static isSeq: typeof isSeq,
}

declare export function isSeq(v: mixed): boolean %checks(v instanceof Seq);
