/**
 * Regression test for annotation_inference AnnotSpecializeT match ordering fix.
 *
 * Tests extending .constructor from an imported instance, mirroring
 * the OCLinking.js pattern: class OCLinking extends Linking.constructor
 *
 * Before the fix, this produced:
 *   Cannot use class extends on function [1] in an export position.
 *   [invalid-exported-annotation]
 */

const emitter = require('./emitter');

class CustomEmitter extends emitter.constructor {
  customMethod(): string {
    return "custom";
  }
}

module.exports = CustomEmitter;
