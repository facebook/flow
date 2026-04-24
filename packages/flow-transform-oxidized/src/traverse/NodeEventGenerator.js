/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

import type {ESNode} from 'flow-estree-oxidized';
import type {ESQueryOptions, Selector} from './esquery';
import type {SafeEmitter} from './SafeEmitter';

import {FlowVisitorKeys} from 'flow-parser-oxidized';
import * as esquery from './esquery';

type ParsedSelector = $ReadOnly<{
  /** The string that was parsed into this selector */
  rawSelector: string,
  /** `true` if this should be emitted when exiting the node rather than when entering */
  isExit: boolean,
  /** An object (from esquery) describing the matching behavior of the selector */
  parsedSelector: Selector,
  /** A list of node types that could possibly cause the selector to match, or `null` if all node types could cause a match */
  listenerTypes: ?Array<ESNode['type']>,
  /** The total number of classes, pseudo-classes, and attribute queries in this selector */
  attributeCount: number,
  /** The total number of identifier queries in this selector */
  identifierCount: number,
}>;

const ESQUERY_OPTIONS: ESQueryOptions = Object.freeze({
  visitorKeys: FlowVisitorKeys,
  fallback: (node: ESNode) => {
    throw new Error(`No visitor keys found for node type "${node.type}".`);
  },
});

/**
 * Computes the union of one or more arrays
 * @param arrays One or more arrays to union
 * @returns The union of the input arrays
 */
function union<T>(...arrays: Array<Array<T>>): Array<T> {
  return [...new Set(arrays.flat())];
}

/**
 * Computes the intersection of one or more arrays
 * @param arrays One or more arrays to intersect
 * @returns The intersection of the input arrays
 */
function intersection<T>(...arrays: Array<Array<T>>): Array<T> {
  if (arrays.length === 0) {
    return [];
  }

  let result = [...new Set(arrays[0])];

  for (const array of arrays.slice(1)) {
    result = result.filter(x => array.includes(x));
  }
  return result;
}

/**
 * Gets the possible types of a selector
 * @param parsedSelector An object (from esquery) describing the matching behavior of the selector
 * @returns The node types that could possibly trigger this selector, or `null` if all node types could trigger it
 */
function getPossibleTypes(parsedSelector: Selector): ?Array<ESNode['type']> {
  switch (parsedSelector.type) {
    case 'identifier':
      if (!(parsedSelector.value in FlowVisitorKeys)) {
        throw new Error(`Unexpected selector ${parsedSelector.value}`);
      }
      // $FlowExpectedError[incompatible-type]
      return [parsedSelector.value];

    case 'matches': {
      const typesForComponents = parsedSelector.selectors.map(getPossibleTypes);
      const typesForComponentsNonNull = typesForComponents.filter(Boolean);

      if (typesForComponents.length === typesForComponentsNonNull.length) {
        return union(...typesForComponentsNonNull);
      }
      return null;
    }

    case 'compound': {
      const typesForComponents = parsedSelector.selectors
        .map(getPossibleTypes)
        .filter(Boolean);

      // If all of the components could match any type, then the compound could also match any type.
      if (!typesForComponents.length) {
        return null;
      }

      /*
       * If at least one of the components could only match a particular type, the compound could only match
       * the intersection of those types.
       */
      return intersection(...typesForComponents);
    }

    case 'child':
    case 'descendant':
    case 'sibling':
    case 'adjacent':
      return getPossibleTypes(parsedSelector.right);

    case 'class':
      if (parsedSelector.name === 'function') {
        return [
          'FunctionDeclaration',
          'FunctionExpression',
          'ArrowFunctionExpression',
        ];
      }

      return null;

    default:
      return null;
  }
}

/**
 * Counts the number of class, pseudo-class, and attribute queries in this selector
 * @param parsedSelector An object (from esquery) describing the selector's matching behavior
 * @returns The number of class, pseudo-class, and attribute queries in this selector
 */
function countClassAttributes(parsedSelector: Selector): number {
  switch (parsedSelector.type) {
    case 'child':
    case 'descendant':
    case 'sibling':
    case 'adjacent':
      return (
        countClassAttributes(parsedSelector.left) +
        countClassAttributes(parsedSelector.right)
      );

    case 'compound':
    case 'not':
    case 'matches':
      return parsedSelector.selectors.reduce(
        (sum, childSelector) => sum + countClassAttributes(childSelector),
        0,
      );

    case 'attribute':
    case 'field':
    case 'nth-child':
    case 'nth-last-child':
      return 1;

    default:
      return 0;
  }
}

/**
 * Counts the number of identifier queries in this selector
 * @param parsedSelector An object (from esquery) describing the selector's matching behavior
 * @returns The number of identifier queries
 */
function countIdentifiers(parsedSelector: Selector): number {
  switch (parsedSelector.type) {
    case 'child':
    case 'descendant':
    case 'sibling':
    case 'adjacent':
      return (
        countIdentifiers(parsedSelector.left) +
        countIdentifiers(parsedSelector.right)
      );

    case 'compound':
    case 'not':
    case 'matches':
      return parsedSelector.selectors.reduce(
        (sum, childSelector) => sum + countIdentifiers(childSelector),
        0,
      );

    case 'identifier':
      return 1;

    default:
      return 0;
  }
}

/**
 * Compares the specificity of two selector objects, with CSS-like rules.
 * @param selectorA An AST selector descriptor
 * @param selectorB Another AST selector descriptor
 * @returns
 * a value less than 0 if selectorA is less specific than selectorB
 * a value greater than 0 if selectorA is more specific than selectorB
 * a value less than 0 if selectorA and selectorB have the same specificity, and selectorA <= selectorB alphabetically
 * a value greater than 0 if selectorA and selectorB have the same specificity, and selectorA > selectorB alphabetically
 */
function compareSpecificity(
  selectorA: ParsedSelector,
  selectorB: ParsedSelector,
): number {
  return (
    selectorA.attributeCount - selectorB.attributeCount ||
    selectorA.identifierCount - selectorB.identifierCount ||
    (selectorA.rawSelector <= selectorB.rawSelector ? -1 : 1)
  );
}

/**
 * Parses a raw selector string, and throws a useful error if parsing fails.
 * @param rawSelector A raw AST selector
 * @returns An object (from esquery) describing the matching behavior of this selector
 * @throws An error if the selector is invalid
 */
function tryParseSelector(rawSelector: string): Selector {
  try {
    return esquery.parse(rawSelector.replace(/:exit$/u, ''));
  } catch (err) {
    if (
      err.location &&
      err.location.start &&
      typeof err.location.start.offset === 'number'
    ) {
      throw new SyntaxError(
        `Syntax error in selector "${rawSelector}" at position ${err.location.start.offset}: ${err.message}`,
      );
    }
    throw err;
  }
}

const selectorCache = new Map<string, ParsedSelector>();

/**
 * Parses a raw selector string, and returns the parsed selector along with specificity and type information.
 * @param rawSelector A raw AST selector
 * @returns A selector descriptor
 */
function parseSelector(rawSelector: string): ParsedSelector {
  const cachedSelector = selectorCache.get(rawSelector);
  if (cachedSelector) {
    return cachedSelector;
  }

  const parsedSelector = tryParseSelector(rawSelector);

  const result: ParsedSelector = {
    rawSelector,
    isExit: rawSelector.endsWith(':exit'),
    parsedSelector,
    listenerTypes: getPossibleTypes(parsedSelector),
    attributeCount: countClassAttributes(parsedSelector),
    identifierCount: countIdentifiers(parsedSelector),
  };

  selectorCache.set(rawSelector, result);
  return result;
}

/**
 * The event generator for AST nodes.
 */
export class NodeEventGenerator {
  +emitter: SafeEmitter;
  +_currentAncestry: Array<ESNode> = [];
  +_enterSelectorsByNodeType: Map<ESNode['type'], Array<ParsedSelector>> =
    new Map();
  +_exitSelectorsByNodeType: Map<ESNode['type'], Array<ParsedSelector>> =
    new Map();
  +_anyTypeEnterSelectors: Array<ParsedSelector> = [];
  +_anyTypeExitSelectors: Array<ParsedSelector> = [];

  /**
   * @param emitter
   * An SafeEmitter which is the destination of events. This emitter must already
   * have registered listeners for all of the events that it needs to listen for.
   * (See lib/linter/safe-emitter.js for more details on `SafeEmitter`.)
   */
  constructor(emitter: SafeEmitter) {
    this.emitter = emitter;

    emitter.eventNames().forEach(rawSelector => {
      const selector = parseSelector(rawSelector);

      if (selector.listenerTypes) {
        const typeMap = selector.isExit
          ? this._exitSelectorsByNodeType
          : this._enterSelectorsByNodeType;

        selector.listenerTypes.forEach(nodeType => {
          const selectors = typeMap.get(nodeType);
          if (!selectors) {
            typeMap.set(nodeType, [selector]);
          } else {
            selectors.push(selector);
          }
        });
      } else {
        const selectors = selector.isExit
          ? this._anyTypeExitSelectors
          : this._anyTypeEnterSelectors;

        selectors.push(selector);
      }
    });

    this._anyTypeEnterSelectors.sort(compareSpecificity);
    this._anyTypeExitSelectors.sort(compareSpecificity);
    this._enterSelectorsByNodeType.forEach(selectorList =>
      selectorList.sort(compareSpecificity),
    );
    this._exitSelectorsByNodeType.forEach(selectorList =>
      selectorList.sort(compareSpecificity),
    );
  }

  /**
   * Checks a selector against a node, and emits it if it matches
   * @param node The node to check
   * @param selector An AST selector descriptor
   * @private
   */
  _applySelector(node: ESNode, selector: ParsedSelector): void {
    if (
      esquery.matches(
        node,
        selector.parsedSelector,
        this._currentAncestry,
        ESQUERY_OPTIONS,
      )
    ) {
      this.emitter.emit(selector.rawSelector, node);
    }
  }

  /**
   * Applies all appropriate selectors to a node, in specificity order
   * @param node The node to check
   * @param isExit `false` if the node is currently being entered, `true` if it's currently being exited
   * @private
   */
  _applySelectors(node: ESNode, isExit: boolean): void {
    const selectorsByNodeType =
      (isExit
        ? this._exitSelectorsByNodeType
        : this._enterSelectorsByNodeType
      ).get(node.type) || [];
    const anyTypeSelectors = isExit
      ? this._anyTypeExitSelectors
      : this._anyTypeEnterSelectors;

    /*
     * selectorsByNodeType and anyTypeSelectors were already sorted by specificity in the constructor.
     * Iterate through each of them, applying selectors in the right order.
     */
    let selectorsByTypeIndex = 0;
    let anyTypeSelectorsIndex = 0;

    while (
      selectorsByTypeIndex < selectorsByNodeType.length ||
      anyTypeSelectorsIndex < anyTypeSelectors.length
    ) {
      if (
        selectorsByTypeIndex >= selectorsByNodeType.length ||
        (anyTypeSelectorsIndex < anyTypeSelectors.length &&
          compareSpecificity(
            anyTypeSelectors[anyTypeSelectorsIndex],
            selectorsByNodeType[selectorsByTypeIndex],
          ) < 0)
      ) {
        this._applySelector(node, anyTypeSelectors[anyTypeSelectorsIndex++]);
      } else {
        this._applySelector(node, selectorsByNodeType[selectorsByTypeIndex++]);
      }
    }
  }

  /**
   * Emits an event of entering AST node.
   * @param node A node which was entered.
   */
  enterNode(node: ESNode): void {
    this._applySelectors(node, false);
    this._currentAncestry.unshift(node);
  }

  /**
   * Emits an event of leaving AST node.
   * @param node A node which was left.
   */
  leaveNode(node: ESNode): void {
    this._currentAncestry.shift();
    this._applySelectors(node, true);
  }
}
