/**
 * Regression test for annotation_inference AnnotSpecializeT match ordering fix.
 *
 * Tests that using addEventListener on a class that extends .constructor
 * of an instance does not produce missing-local-annot errors.
 *
 * This was a downstream effect: when .constructor produced
 * invalid-exported-annotation, the type of the resulting class was broken,
 * causing missing-local-annot on callback parameters.
 *
 * @flow strict-local
 */

const CustomEmitter = require('./class_extends_imported_constructor');

const instance = new CustomEmitter();

const subscription = instance.addEventListener('url', event => {
  // Before the fix, 'event' would get missing-local-annot
  const _e = event;
});
