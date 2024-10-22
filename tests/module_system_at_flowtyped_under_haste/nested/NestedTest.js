import * as foo from 'foo';
import * as bar from 'bar/baz';
import * as scopedFoo from '@scoped/foo';
import * as conflict from 'conflict';

foo.foo as string; // ok
foo.foo as empty; // error
bar.foo as string; // ok
bar.foo as empty; // error
scopedFoo.foo as string; // ok
scopedFoo.foo as empty; // error
conflict.foo as string; // ok
conflict.foo as number; // error
