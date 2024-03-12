// @flow

function foo<T, S>(x: T, y: S) {
    const z = { ...x, prop: 1, ...y };
//        ^
}
