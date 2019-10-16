//@flow

import { foo } from 'foo_fail';

(foo: 'this is foo'); // no error
