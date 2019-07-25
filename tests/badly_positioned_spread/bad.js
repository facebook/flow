// @flow

const Foo = require('./foo');

const y : {foo: {color?: string}} = {foo: {color: "cat"}};
const x = {
    Bar: {
        foo: Foo,
    },
    ...{Bar: y}
};
