/**
 * @format
 * @flow
 */

type O = {p: empty};
const o1 = {p: 42};
const o2 = {p: 42};
([o1]: [O]);
([o2]: [{p: empty | empty}] | [{p: empty | empty}]);
