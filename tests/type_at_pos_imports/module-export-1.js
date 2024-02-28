// @flow

module.exports = 1;
//     ^ --pretty
(module.exports: number);
//      ^ --pretty

if (0 < 1) {
  module.exports = "blah";
//       ^ --pretty
  (module.exports: string);
//        ^ --pretty
}
(module.exports: number | string);
//      ^ --pretty
