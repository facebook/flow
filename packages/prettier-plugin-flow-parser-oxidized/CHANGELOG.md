## 0.35.0
- Support `readonly` as a Flow variance annotation, equivalent to `+` (covariant/read-only). See [PR #19022 in prettier](https://github.com/prettier/prettier/pull/19022).
- Updated hermes-parser to 0.35.0.

## 0.34.1
- Fixed a bug that a white space was accidentally removed by autoformatter causing prettier to add unnecessary new lines.

## 0.34.0
- Support Flow's new `keyof` operator that replaces `$Keys`. See [this PR in prettier main](https://github.com/prettier/prettier/pull/18801/changes).

## 0.33.2
- Reverts base Prettier version back to 3.6.2, but keeps other changes

## 0.33.1
- Includes changes up to Prettier 3.7.4
- Support Flow match instance patterns
- Support Flow Records

## 0.32.0
- Support opaque type with both lower and upper bounds with `super` and `extends` syntax.

## 0.31.1
- Rebuild based on `@prettier/plugin-hermes`. There should be no formatting differences, but it will be less buggy.

## Previous Versions
For versions before 0.31.1, see [hermes-parser changelog](https://github.com/facebook/hermes/blob/static_h/tools/hermes-parser/js/CHANGELOG.md).
