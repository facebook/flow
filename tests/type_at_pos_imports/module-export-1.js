// @flow

module.exports = 1;
//     ^
module.exports as number;
//      ^

if (0 < 1) {
  module.exports = "blah";
//       ^
  module.exports as string;
//        ^
}
module.exports as number | string;
//      ^
