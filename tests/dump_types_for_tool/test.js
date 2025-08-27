//@flow
import {type Foo} from './exp';
function foo(x: number) { }
foo(0);

// test deduping of inferred types
function nullToUndefinedWrapper(f: (?Object | ?Object | ?string | ?string) => mixed) {}
nullToUndefinedWrapper((val) => val === null ? undefined : val);
