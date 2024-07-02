// @flow

declare component Foo();
declare var rendersFoo: renders Foo;
declare var rendersFooMaybe: renders? Foo;
declare var rendersFooStar: renders* Foo;

export function foo() {
    return {
        rendersFoo,
        rendersFooMaybe,
        rendersFooStar,
    }
}
