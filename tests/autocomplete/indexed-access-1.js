// @flow
const x = 'x'; // should NOT show up in result
/** @deprecated */
type K = 'foo'; // should show up in result

type O = {/** @deprecated */foo: number, bar?: string};

type T = O[
//         ^
