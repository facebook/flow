// @flow
// Exports _should_ only appear at the top-level, but this is only enforced in our prelude libdefs
// and not our real ones. If this bug is fixed, a fix must also come for the new env, which does not
// model this constraint.

{
  exports.foo = 5;
}
