// @flow

module.exports = 1;
//     ^ --pretty
module.exports as number;
//      ^ --pretty

if (0 < 1) {
  module.exports = "blah";
//       ^ --pretty
  module.exports as string;
//        ^ --pretty
}
module.exports as number | string;
//      ^ --pretty
