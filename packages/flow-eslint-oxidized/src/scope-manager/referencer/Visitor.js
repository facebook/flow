/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {ESNode} from 'flow-estree-oxidized';
import type {
  PatternVisitorIdentifierCallback,
  PatternVisitorTypeAnnotationCallback,
  PatternVisitorOptions,
} from './PatternVisitor';
import type {VisitorOptions} from './VisitorBase';

import {VisitorBase} from './VisitorBase';
import {PatternVisitor} from './PatternVisitor';

type VisitPatternOptions = $ReadOnly<{
  ...PatternVisitorOptions,
  processRightHandNodes?: boolean,
}>;

class Visitor extends VisitorBase {
  +_options: VisitorOptions;
  constructor(optionsOrVisitor: VisitorOptions | Visitor) {
    super(
      optionsOrVisitor instanceof Visitor
        ? optionsOrVisitor._options
        : optionsOrVisitor,
    );

    this._options =
      optionsOrVisitor instanceof Visitor
        ? optionsOrVisitor._options
        : optionsOrVisitor;
  }

  visitPattern(
    node: ESNode,
    identifierCallback: PatternVisitorIdentifierCallback,
    typeAnnotationCallback: PatternVisitorTypeAnnotationCallback,
    options: VisitPatternOptions = {processRightHandNodes: false},
  ): void {
    // Call the callback at left hand identifier nodes, and Collect right hand nodes.
    const visitor = new PatternVisitor(
      this._options,
      node,
      identifierCallback,
      typeAnnotationCallback,
    );

    visitor.visit(node);

    // Process the right hand nodes recursively.
    if (options.processRightHandNodes === true) {
      this.visitArray(visitor.rightHandNodes);
    }
  }
}

export type {VisitorOptions};
export {Visitor, VisitorBase};
