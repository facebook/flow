// @flow

import { val1 } from 'foo';
val1 as number; // OK initially (@types says number), ERROR after foo gains own types (string)
val1 as string; // ERROR initially (@types says number), OK after foo gains own types (string)
