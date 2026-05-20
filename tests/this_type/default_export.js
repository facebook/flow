// Default-exported declare class — used by `default_import.js`.
// Exercises the cross-module [import_default_for_extends] /
// [Name_def.Default] path so polymorphic `this` survives the
// canonicalize-on-import unwrap when the parent is imported via
// `import X from '...'`.
declare class CDefault {
  clone(): this;
}
export default CDefault;
