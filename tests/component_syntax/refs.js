component Reffed() { return null }

component Foo(ref: {current: typeof Reffed}) { return null };

component Bar(ref: ((typeof Reffed) => mixed)) { return null };


component Baz(ref: React$RefSetter<typeof Reffed>) { return null };

component Qux(ref: React$RefSetter<'div'>) { return null };

(Foo: React$AbstractComponent<empty, string>); // err: React.RefSetter<string> ~> {current: typeof Reffed}
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

component Bad<T>(ref: React$RefSetter<T>) { return null };

component Bad2<T>(ref: React$RefSetter<Array<Array<Array<T>>>>) { return null };

{
  declare component Foo(ref?: React.RefSetter<{foo: string, bar: number}>)

  declare const badRef: React.RefSetter<{foo: string, bar: boolean}>;
  declare const goodRef: React.RefSetter<{foo: string, bar: number}>;
  <Foo ref={badRef} />; // error
  <Foo ref={goodRef} />; // ok
  <Foo ref={null} />; // ok
  <Foo ref={undefined} />; // ok
  <Foo />; // ok
}
