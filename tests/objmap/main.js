// @flow

import type {SpreadKeyMirroredProps} from './keyMirror';

({b: 'b'}: SpreadKeyMirroredProps); // okay

import { t1, type OptionalProps } from './objmapconst';

(t1: string); // error
(t1: boolean);

({b: true}: OptionalProps); // okay
