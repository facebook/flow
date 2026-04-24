/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @noformat
 */

'use strict';

import type {VisitorOptions} from './VisitorBase';
import type {
  ArrayExpression,
  ArrayPattern,
  AssignmentExpression,
  AssignmentPattern,
  BindingName,
  CallExpression,
  ESNode,
  Identifier,
  MemberExpression,
  ObjectPattern,
  Property,
  RestElement,
  SpreadElement,
  TypeAnnotation,
} from 'flow-estree-oxidized';

import {VisitorBase} from './VisitorBase';

type PatternVisitorIdentifierCallback = (
  pattern: Identifier,
  info: {
    assignments: Array<AssignmentPattern | AssignmentExpression>,
    rest: boolean,
    topLevel: boolean,
  },
) => void;
type PatternVisitorTypeAnnotationCallback = (
  typeAnnotation: TypeAnnotation,
  pattern: BindingName,
) => void;


function isPattern(node: ESNode): boolean {
  return (
    node.type === 'Identifier' ||
    node.type === 'ObjectPattern' ||
    node.type === 'ArrayPattern' ||
    node.type === 'SpreadElement' ||
    node.type === 'RestElement' ||
    node.type === 'AssignmentPattern'
  );
}

type PatternVisitorOptions = VisitorOptions;

class PatternVisitor extends VisitorBase {
  static isPattern: typeof isPattern = isPattern;

  +_rootPattern: ESNode;
  +_identifierCallback: PatternVisitorIdentifierCallback;
  +_typeAnnotationCallback: PatternVisitorTypeAnnotationCallback;
  +_assignments: Array<AssignmentPattern | AssignmentExpression> = [];
  +rightHandNodes: Array<ESNode> = [];
  +_restElements: Array<RestElement> = [];

  constructor(
    options: PatternVisitorOptions,
    rootPattern: ESNode,
    identifierCallback: PatternVisitorIdentifierCallback,
    typeAnnotationCallback: PatternVisitorTypeAnnotationCallback,
  ) {
    super(options);
    this._rootPattern = rootPattern;
    this._identifierCallback = identifierCallback;
    this._typeAnnotationCallback = typeAnnotationCallback;
  }

  //
  // Visitors
  //

  ArrayExpression(node: ArrayExpression): void {
    this.visitArray(node.elements);
  }

  ArrayPattern(pattern: ArrayPattern): void {
    for (const element of pattern.elements) {
      this.visit(element);
    }
    if (pattern.typeAnnotation) {
      this._typeAnnotationCallback(pattern.typeAnnotation, pattern);
    }
  }

  AssignmentExpression(node: AssignmentExpression): void {
    this._assignments.push(node);
    this.visit(node.left);
    this.rightHandNodes.push(node.right);
    this._assignments.pop();
  }

  AssignmentPattern(pattern: AssignmentPattern): void {
    this._assignments.push(pattern);
    this.visit(pattern.left);
    this.rightHandNodes.push(pattern.right);
    this._assignments.pop();
  }

  CallExpression(node: CallExpression): void {
    // arguments are right hand nodes.
    node.arguments.forEach(a => {
      this.rightHandNodes.push(a);
    });
    this.visit(node.callee);
  }

  Identifier(pattern: Identifier): void {
    const lastRestElement =
      this._restElements[this._restElements.length - 1] ?? null;

    this._identifierCallback(pattern, {
      topLevel: pattern === this._rootPattern,
      rest: lastRestElement != null && lastRestElement.argument === pattern,
      assignments: this._assignments,
    });
    if (pattern.typeAnnotation) {
      this._typeAnnotationCallback(pattern.typeAnnotation, pattern);
    }
  }

  MemberExpression(node: MemberExpression): void {
    // Computed property's key is a right hand node.
    if (node.computed === true) {
      this.rightHandNodes.push(node.property);
    }

    // the object is only read, write to its property.
    this.rightHandNodes.push(node.object);
  }

  ObjectPattern(pattern: ObjectPattern): void {
    for (const property of pattern.properties) {
      this.visit(property);
    }
    if (pattern.typeAnnotation) {
      this._typeAnnotationCallback(pattern.typeAnnotation, pattern);
    }
  }

  Property(property: Property): void {
    // Computed property's key is a right hand node.
    if (property.computed) {
      this.rightHandNodes.push(property.key);
    }

    // If it's shorthand, its key is same as its value.
    // If it's shorthand and has its default value, its key is same as its value.left (the value is AssignmentPattern).
    // If it's not shorthand, the name of new variable is its value's.
    this.visit(property.value);
  }

  RestElement(pattern: RestElement): void {
    this._restElements.push(pattern);
    this.visit(pattern.argument);
    this._restElements.pop();
  }

  SpreadElement(node: SpreadElement): void {
    this.visit(node.argument);
  }

  TypeAnnotation(_node: TypeAnnotation): void {
    // we don't want to visit types implicitly
  }
}

export type {
  PatternVisitorIdentifierCallback,
  PatternVisitorTypeAnnotationCallback,
  PatternVisitorOptions,
};
export {PatternVisitor};
