// @flow

import { props } from './props';

export class OK {}

export function foo() {
  return {
    ok: new OK,
    props: props(),
  };
}

declare export function bar(): { ok: OK };
