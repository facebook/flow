import * as React from 'react';

export component Poly<T>(foo: T, bar: [T], ...props: { baz?: (T) => void}) {
    return null;
}

<Poly foo={1} bar={[2]} />;
<Poly foo={"a"} bar={["a"]} />;
<Poly foo={1} bar={["a"]} />;
<Poly foo={1} bar={[1]} baz={(x: string) => {}} />; //error

declare const x: Poly<number>;
(x: Poly<string>);

component BRB<TValue: string>(foo: TValue) { return null };

type BRBT<TValue: string> = component(foo:TValue);

component Foo<TValue: string>(setTV: TValue => void, children: React.ChildrenArray<React.Element<BRBT<TValue>>>) {
    return null;
 }

component Test() {
    declare const set: 'number' => void;
    return (<Foo setTV={set /* error */}>
        <BRB foo={'out'} />
    </Foo>)
}

declare const C: React.AbstractComponent<{foo: string}>;
const el = <C foo="str" />;
el.props.bar; // error
const el_empty: React.Element<React$AbstractComponent<empty>> = el;
el_empty.props.bar; // error
