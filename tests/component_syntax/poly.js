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
