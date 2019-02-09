//@flow

'use strict';

import type {Props} from './lib.js';

class Class {
  constructor(props: Props) {
    const inputProps = {
      foo: 'bar',
      ...props,
    };
  }
}

declare var x : mixed;
//$FlowFixMe should suppress
if(x) {

};
