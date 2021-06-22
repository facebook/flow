// @flow
const x = 'x'; // should NOT show up in result
type K = 'foo'; // should show up in result

type O = {foo: number, bar?: string};

type T = O[
//         ^
