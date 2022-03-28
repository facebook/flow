require('XXX'); // ERROR - no custom message

require('XXX.foo'); // ERROR - suggest `make-foo`

require('XXX.bar'); // ERROR - suggest `build bar`

// Type import
import type A from 'XXX.foo'; // ERROR - suggest `make-foo`

import type B from 'XXX.bar'; // ERROR - suggest `build bar`
