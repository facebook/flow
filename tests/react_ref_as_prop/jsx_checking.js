import {FnWithoutRefProp, FnWithOptionalRefProp, FnWithRequiredRefProp} from './fn_components';
import {CompWithoutRefProp, CompWithOptionalRefProp, CompWithRequiredRefProp} from './component_syntax_components';

<FnWithoutRefProp foo="bar" />; // ok
<FnWithOptionalRefProp foo="bar" />; // ok
<FnWithRequiredRefProp foo="bar" />; // error: missing ref prop
<FnWithoutRefProp foo={1} />; // error: 1 ~> string, normal checking still works
<FnWithoutRefProp foo="bar" ref={(_: ?HTMLElement) => {}} />; // error: extra ref prop
<FnWithOptionalRefProp foo="bar" ref={(_: ?HTMLElement) => {}} />; // ok
<FnWithOptionalRefProp foo="bar" ref={(_: ?string) => {}} />; // error: string ~> HTMLElement, ref checking still works
<FnWithRequiredRefProp foo="bar" ref={(_: ?HTMLElement) => {}} />; // ok
<FnWithRequiredRefProp foo="bar" ref={(_: ?string) => {}} />; // error: string ~> HTMLElement, ref checking still works

<CompWithoutRefProp foo="bar" />; // ok
<CompWithOptionalRefProp foo="bar" />; // ok
<CompWithRequiredRefProp foo="bar" />; // no error for now, should have prop-missing error under full support
<CompWithoutRefProp foo={1} />; // error: 1 ~> string, normal checking still works
<CompWithoutRefProp foo="bar" ref={(_: ?HTMLElement) => {}} />; // error: extra ref prop
<CompWithOptionalRefProp foo="bar" ref={(_: ?HTMLElement) => {}} />; // ok
<CompWithOptionalRefProp foo="bar" ref={(_: ?string) => {}} />; // error: string ~> HTMLElement, ref checking still works
<CompWithRequiredRefProp foo="bar" ref={(_: ?HTMLElement) => {}} />; // ok
<CompWithRequiredRefProp foo="bar" ref={(_: ?string) => {}} />; // error: string ~> HTMLElement, ref checking still works
