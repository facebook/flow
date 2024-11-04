import {FnWithoutRefProp, FnWithOptionalRefProp, FnWithRequiredRefProp} from './fn_components';

<FnWithoutRefProp foo="bar" />; // ok
<FnWithOptionalRefProp foo="bar" />; // ok
<FnWithRequiredRefProp foo="bar" />; // error: missing ref prop
<FnWithoutRefProp foo={1} />; // error: 1 ~> string, normal checking still works
<FnWithoutRefProp foo="bar" ref={(_: ?HTMLElement) => {}} />; // error: extra ref prop
<FnWithOptionalRefProp foo="bar" ref={(_: ?HTMLElement) => {}} />; // ok
<FnWithOptionalRefProp foo="bar" ref={(_: ?string) => {}} />; // error: string ~> HTMLElement, ref checking still works
<FnWithRequiredRefProp foo="bar" ref={(_: ?HTMLElement) => {}} />; // ok
<FnWithRequiredRefProp foo="bar" ref={(_: ?string) => {}} />; // error: string ~> HTMLElement, ref checking still works
