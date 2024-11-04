import {FnWithoutRefProp,FnWithOptionalRefProp,FnWithRequiredRefProp} from './fn_components';

declare function id<Props: {...}, Instance>(
  c: component(ref: React.RefSetter<Instance>, ...Props),
): component(ref: React.RefSetter<Instance>, ...Props);

const IdMappedFnWithoutRefProp = id(FnWithoutRefProp); // ok
const IdMappedFnWithOptionalRefProp = id(FnWithOptionalRefProp); // ok
const IdMappedFnWithRequiredRefProp = id(FnWithRequiredRefProp); // ok
({foo: ''}) as React.ElementConfig<typeof IdMappedFnWithoutRefProp>; // ok
({foo: ''}) as React.ElementConfig<typeof IdMappedFnWithOptionalRefProp>; // ok
({foo: ''}) as React.ElementConfig<typeof IdMappedFnWithRequiredRefProp>; // ok
({}) as React.ElementConfig<typeof IdMappedFnWithoutRefProp>; // error: missing foo prop
({}) as React.ElementConfig<typeof IdMappedFnWithOptionalRefProp>; // error: missing foo prop
({}) as React.ElementConfig<typeof IdMappedFnWithRequiredRefProp>; // error: missing foo prop
undefined as React.ElementRef<typeof IdMappedFnWithoutRefProp>; // ok
null as React.ElementRef<typeof IdMappedFnWithoutRefProp>; // error: null ~> void
new HTMLElement() as React.ElementRef<typeof IdMappedFnWithOptionalRefProp>; // ok
null as React.ElementRef<typeof IdMappedFnWithOptionalRefProp>; // error: null ~> HTMLElement
new HTMLElement() as React.ElementRef<typeof IdMappedFnWithRequiredRefProp>; // ok
null as React.ElementRef<typeof IdMappedFnWithRequiredRefProp>; // error: null ~> HTMLElement

id((props) => { props as {+foo: string} }) as React.ComponentType<{foo: string}>; // ok
id((props) => { props as {+foo: string} }) as component(foo: string); // ok
