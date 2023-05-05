// @flow

export {foo} from "./helpers/exports_named.js";
//      ^
export {foo as foo1} from "./helpers/exports_named.js";
// ^
export {foo as foo2} from "./helpers/exports_named.js";
//       ^
export {foo as foo3} from "./helpers/exports_named.js";
//          ^
export {foo as foo4} from "./helpers/exports_named.js";
//             ^
export {foo as foo5} from "./helpers/exports_named.js";
//                    ^
export {foo as foo6} from "./helpers/exports_named.js";
//                            ^
export {foo as lib_foo1} from 'test_lib';
//      ^
export {foo as lib_foo2} from 'test_lib';
//             ^
export {foo as lib_foo3} from 'test_lib';
//                              ^

const bar = 0
export {bar}
//      ^
export {bar as bar1}
//      ^
export {bar as bar2}
//          ^
export {bar as bar3}
//             ^
