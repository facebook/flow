// @flow

import {f} from './cycle2';

declare export function g(): any;

export default f(); // annotated with $FlowFixMe because f has an annotated $FlowFixMe return
