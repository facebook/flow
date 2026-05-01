import type {ObjFoo} from './test';

true as ObjFoo; // OK
1 as ObjFoo; // Error

import type {B, N, S} from './optional_export';

true as B; // OK
undefined as B; // OK
1 as B; // Error

1 as N; // OK
undefined as N; // OK
true as N; // Error

'hi' as S; // OK
undefined as S; // OK
true as S; // Error
