/**
 * Test interaction of object intersections and predicates.
 * Definitions in lib/lib.js
 *
 * @flow
 */

type DuplexStreamOptions = ReadableStreamOptions & WritableStreamOptions & {
  allowHalfOpen? : boolean,
  readableObjectMode? : boolean,
  writableObjectMode? : boolean
};

function hasObjectMode(options: DuplexStreamOptions): boolean {
  return options.objectMode
    || options.readableObjectMode
    || options.writableObjectMode;
}
