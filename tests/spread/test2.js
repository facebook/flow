/**
 * @flow
 */

function foo(o: {foo: number}) {
    bar({...o});
}
function bar(_: {foo:number}) { }
foo({foo: 42});
