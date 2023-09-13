component Reffed() { return null }

component Foo(ref: {current: typeof Reffed}) { return null };

component Bar(ref: ((typeof Reffed) => mixed)) { return null };


component Baz(ref: React$RefSetter<typeof Reffed>) { return null };

component Qux(ref: React$RefSetter<'div'>) { return null };

(Foo: React$AbstractComponent<empty, string>); // err
(Foo: React$AbstractComponent<empty, typeof Reffed>); //ok

(Bar: React$AbstractComponent<empty, string>); // err

(Baz: React$AbstractComponent<empty, string>); // err

(Qux: React$AbstractComponent<empty, string>); // ok


import * as React from 'react';
{ // ok
    component MyNestedInput(other: string, ref: React$RefSetter<?HTMLInputElement>) {
        return <input other={other} ref={ref} />
    }

    component MyInput(label: string, ref: React$RefSetter<?HTMLElement>, ...otherProps: { other: string}) {
        return (
            <label>
                {label}
                <MyNestedInput {...otherProps} ref={ref} />
            </label>
            );
    }

    component Form() {
        const ref = React.useRef<?HTMLElement>(null);

        function handleClick() {
            ref.current?.focus();
        }

        return (
            <form>
                <MyInput label="Enter your name:" ref={ref} other="whatever" />
                <button type="button" onClick={handleClick}>
                    Edit
                </button>
            </form>
        );
    }
}

{ //mismatch
    component MyNestedInput(other: string, ref: React$RefSetter<?HTMLElement>) {
        return <input other={other} ref={ref} />
    }

    component MyInput(label: string, ref: React$RefSetter<?HTMLInputElement>, ...otherProps: { other: string}) {
        return (
            <label>
                {label}
                <MyNestedInput {...otherProps} ref={ref} />
            </label>
            );
    }

    component Form() {
        const ref = React.useRef<?HTMLElement>(null);

        function handleClick() {
            ref.current?.focus();
        }

        return (
            <form>
                <MyInput label="Enter your name:" ref={ref} other="whatever" />
                <button type="button" onClick={handleClick}>
                    Edit
                </button>
            </form>
        );
    }
}
