declare opaque type Kind super 'foo' | 'bar' extends string;

declare const kind: Kind;
'foo' as Kind; // ok
'bar' as Kind; // ok
'baz' as Kind; // error
kind as string; // ok
kind as number; // error

declare opaque type Obj super {foo: string} extends {...};
({foo: ''}) as Obj; // ok
({}) as Obj; // error
1 as Obj; // error
declare const obj: Obj;
({...obj}) as {...}; // ok

declare opaque type InvalidNoRepr super string extends 'foo'; // error: string ~> 'foo'
opaque type InvalidRepr super string extends 'foo' = 'foo'; // error: string ~> 'foo'

import type {BothBounds} from './opaque_exports';
'a' as BothBounds; // ok
'b' as BothBounds; // error
