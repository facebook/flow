// @flow

type O = {
    prop1?: "foo",
    prop2: ?"foo",
};

const x: O = {
    prop1: ,
//        ^
    prop2: ,
//        ^
};
