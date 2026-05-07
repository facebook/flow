// @flow

declare component Foo();
declare const rendersFoo: renders Foo;
declare const rendersFooMaybe: renders? Foo;
declare const rendersFooStar: renders* Foo;

export function foo() {
    return {
        rendersFoo,
        rendersFooMaybe,
        rendersFooStar,
    }
}
