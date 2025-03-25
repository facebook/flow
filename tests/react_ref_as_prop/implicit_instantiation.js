import {FnWithoutRefProp,FnWithOptionalRefProp,FnWithRequiredRefProp} from './fn_components';
import {CompWithoutRefProp, CompWithOptionalRefProp, CompWithRequiredRefProp} from './component_syntax_components';

declare function id<Props: {...}, Ref>(
  c: component(ref?: Ref, ...Props),
): component(ref?: Ref, ...Props);

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

const IdMappedCompWithoutRefProp = id(CompWithoutRefProp); // ok
const IdMappedCompWithOptionalRefProp = id(CompWithOptionalRefProp); // ok
const IdMappedCompWithRequiredRefProp = id(CompWithRequiredRefProp); // ok
({foo: ''}) as React.ElementConfig<typeof IdMappedCompWithoutRefProp>; // ok
({foo: ''}) as React.ElementConfig<typeof IdMappedCompWithOptionalRefProp>; // ok
({foo: ''}) as React.ElementConfig<typeof IdMappedCompWithRequiredRefProp>; // ok
({}) as React.ElementConfig<typeof IdMappedCompWithoutRefProp>; // error: missing foo prop
({}) as React.ElementConfig<typeof IdMappedCompWithOptionalRefProp>; // error: missing foo prop
({}) as React.ElementConfig<typeof IdMappedCompWithRequiredRefProp>; // error: missing foo prop
undefined as React.ElementRef<typeof IdMappedCompWithoutRefProp>; // error: null ~> empty (missing ref prop of IdMappedCompWithoutRefProp becomes empty)
null as React.ElementRef<typeof IdMappedCompWithoutRefProp>; // error: null ~> empty
new HTMLElement() as React.ElementRef<typeof IdMappedCompWithOptionalRefProp>; // ok
null as React.ElementRef<typeof IdMappedCompWithOptionalRefProp>; // error: null ~> HTMLElement
new HTMLElement() as React.ElementRef<typeof IdMappedCompWithRequiredRefProp>; // ok
null as React.ElementRef<typeof IdMappedCompWithRequiredRefProp>; // error: null ~> HTMLElement
