import {FnWithoutRefProp, FnWithOptionalRefProp, FnWithRequiredRefProp} from './fn_components';
import {CompWithoutRefProp, CompWithOptionalRefProp, CompWithRequiredRefProp} from './component_syntax_components';

FnWithoutRefProp as component(foo: string); // ok
FnWithoutRefProp as React.ComponentType<{+foo: string}>; // ok
FnWithoutRefProp as component(foo: string, ref: React.RefSetter<unknown>); // error, since {ref: React.RefSetter<unknown>} ~> {} should be an error

FnWithOptionalRefProp as component(foo: string); // ok
FnWithOptionalRefProp as React.ComponentType<{+foo: string}>; // ok
FnWithOptionalRefProp as component(foo: string, ref: React.RefSetter<unknown>); // ok

FnWithRequiredRefProp as component(foo: string); // error: missing ref prop
FnWithRequiredRefProp as React.ComponentType<{+foo: string}>; // error: missing ref prop
FnWithRequiredRefProp as component(foo: string, ref: React.RefSetter<unknown>); // ok

CompWithoutRefProp as component(foo: string); // ok
CompWithoutRefProp as React.ComponentType<{+foo: string}>; // ok
CompWithoutRefProp as component(foo: string, ref: React.RefSetter<unknown>); // error: React.RefSetter<unknown> ~> void

CompWithOptionalRefProp as component(foo: string); // ok
CompWithOptionalRefProp as React.ComponentType<{+foo: string}>; // ok
CompWithOptionalRefProp as component(foo: string, ref: React.RefSetter<unknown>); // ok

CompWithRequiredRefProp as component(foo: string); // ok
CompWithRequiredRefProp as React.ComponentType<{+foo: string}>; // ok
CompWithRequiredRefProp as component(foo: string, ref: React.RefSetter<unknown>); // ok
