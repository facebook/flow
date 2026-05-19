// @flow

import { val1 } from 'foo';
val1 as string; // OK initially (foo is untyped, no @types/foo, val1 is any)
                // ERROR after @types/foo is added (val1 is number)
                // OK again after @types/foo is removed
