import {FnWithoutRefProp, FnWithOptionalRefProp, FnWithRequiredRefProp} from './fn_components';

FnWithoutRefProp as component(foo: string); // ok
FnWithoutRefProp as React.ComponentType<{+foo: string}>; // ok
// temporarily allowed, should be banned in the full ref-as-prop support,
// since {ref: React.RefSetter<mixed>} ~> {} should be an error
FnWithoutRefProp as component(foo: string, ref: React.RefSetter<mixed>); // ok

FnWithOptionalRefProp as component(foo: string); // ok
FnWithOptionalRefProp as React.ComponentType<{+foo: string}>; // ok
FnWithOptionalRefProp as component(foo: string, ref: React.RefSetter<mixed>); // ok

FnWithRequiredRefProp as component(foo: string); // error: missing ref prop
FnWithRequiredRefProp as React.ComponentType<{+foo: string}>; // ok
FnWithRequiredRefProp as component(foo: string, ref: React.RefSetter<mixed>); // ok
