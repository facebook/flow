import {FnWithoutRefProp, FnWithOptionalRefProp, FnWithRequiredRefProp} from './fn_components';
import {CompWithoutRefProp, CompWithOptionalRefProp, CompWithRequiredRefProp} from './component_syntax_components';

FnWithoutRefProp as component(foo: string); // ok
FnWithoutRefProp as React.ComponentType<{+foo: string}>; // ok
// temporarily allowed, should be banned in the full ref-as-prop support,
// since {ref: React.RefSetter<mixed>} ~> {} should be an error
FnWithoutRefProp as component(foo: string, ref: React.RefSetter<mixed>); // ok

FnWithOptionalRefProp as component(foo: string); // ok
FnWithOptionalRefProp as React.ComponentType<{+foo: string}>; // ok
FnWithOptionalRefProp as component(foo: string, ref: React.RefSetter<mixed>); // ok

FnWithRequiredRefProp as component(foo: string); // error: missing ref prop
FnWithRequiredRefProp as React.ComponentType<{+foo: string}>; // error: missing ref prop
FnWithRequiredRefProp as component(foo: string, ref: React.RefSetter<mixed>); // ok

CompWithoutRefProp as component(foo: string); // ok
CompWithoutRefProp as React.ComponentType<{+foo: string}>; // ok
// temporarily allowed, should be banned in the full ref-as-prop support,
// since {ref: React.RefSetter<mixed>} ~> {} should be an error
CompWithoutRefProp as component(foo: string, ref: React.RefSetter<mixed>); // ok

CompWithOptionalRefProp as component(foo: string); // ok
CompWithOptionalRefProp as React.ComponentType<{+foo: string}>; // ok
CompWithOptionalRefProp as component(foo: string, ref: React.RefSetter<mixed>); // ok

CompWithRequiredRefProp as component(foo: string); // ok
CompWithRequiredRefProp as React.ComponentType<{+foo: string}>; // ok
CompWithRequiredRefProp as component(foo: string, ref: React.RefSetter<mixed>); // ok
