// @flow
// `export * as ns` builds a namespace object, which per spec DOES include the
// source's default. This must be unaffected by the export-star default fix.
export * as inner from './has_default';
