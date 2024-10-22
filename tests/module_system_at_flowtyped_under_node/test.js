import * as foo from 'foo';
import * as bar from 'bar/baz';
import * as scopedFoo from '@scoped/foo';

foo.foo as string; // ok
foo.foo as empty; // error
bar.foo as string; // ok
bar.foo as empty; // error
scopedFoo.foo as string; // ok
scopedFoo.foo as empty; // error
