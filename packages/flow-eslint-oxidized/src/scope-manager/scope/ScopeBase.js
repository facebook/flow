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

import type {
  BlockStatement,
  ESNode,
  Expression,
  Identifier,
  JSXIdentifier,
  Program,
  StringLiteral,
} from 'flow-estree-oxidized';
import type {ClassFieldInitializerScope} from './ClassFieldInitializerScope';
import type {ClassStaticBlockScope} from './ClassStaticBlockScope';
import type {DeclareModuleScope} from './DeclareModuleScope';
import type {DeclareNamespaceScope} from './DeclareNamespaceScope';
import type {ComponentScope} from './ComponentScope';
import type {FunctionScope} from './FunctionScope';
import type {GlobalScope} from './GlobalScope';
import type {ModuleScope} from './ModuleScope';
import type {ScopeTypeType} from './ScopeType';
import type {Scope} from './Scope';
import type {Definition} from '../definition';
import type {
  ReferenceFlagType,
  ReferenceImplicitGlobal,
} from '../referencer/Reference';
import type {ScopeManager} from '../ScopeManager';

import {isStringLiteral} from 'flow-estree-oxidized';
import {ScopeType} from './ScopeType';
import {DefinitionType} from '../definition';
import {createIdGenerator} from '../ID';
import {
  Reference,
  ReferenceFlag,
  ReferenceTypeFlag,
} from '../referencer/Reference';
import {Variable} from '../variable';

/**
 * Test if scope is strict
 */
function isStrictScope(scope: Scope, isMethodDefinition: boolean): boolean {
  // When upper scope exists and is strict, inner scope is also strict.
  if (scope.upper && scope.upper.isStrict) {
    return true;
  }

  if (isMethodDefinition) {
    return true;
  }

  if (scope.type === ScopeType.Class || scope.type === ScopeType.Module) {
    return true;
  }

  if (scope.type === ScopeType.Block || scope.type === ScopeType.Switch) {
    return false;
  }

  const body = ((): false | Program | BlockStatement => {
    if (scope.type === ScopeType.Function) {
      if (scope.block.type === 'ArrowFunctionExpression') {
        if (scope.block.body.type !== 'BlockStatement') {
          return false;
        } else {
          return scope.block.body;
        }
      } else if (scope.block.type === 'Program') {
        return scope.block;
      } else {
        return scope.block.body;
      }
    } else if (scope.type === ScopeType.Global) {
      return scope.block;
    } else {
      return false;
    }
  })();
  if (body === false) {
    return false;
  }

  // Search 'use strict' directive.
  for (let i = 0, iz = body.body.length; i < iz; ++i) {
    const stmt = body.body[i];

    if (stmt.type !== 'ExpressionStatement') {
      break;
    }
    const expr = stmt.expression;

    if (!isStringLiteral(expr)) {
      break;
    }
    return expr.value === 'use strict';
  }

  return false;
}

/**
 * Register scope
 */
function registerScope(scopeManager: ScopeManager, scope: Scope): void {
  scopeManager.scopes.push(scope);

  const scopes = scopeManager.nodeToScope.get(scope.block);

  if (scopes) {
    scopes.push(scope);
  } else {
    scopeManager.nodeToScope.set(scope.block, [scope]);
  }
}

function asScope(scope: ScopeBase<$FlowFixMe, $FlowFixMe, $FlowFixMe>): Scope {
  // $FlowExpectedError[incompatible-type] - it's impossible to tell flow this is safe
  return scope;
}

const generator = createIdGenerator();

type VariableScope =
  | ClassFieldInitializerScope
  | ClassStaticBlockScope
  | ComponentScope
  | FunctionScope
  | GlobalScope
  | ModuleScope
  | DeclareModuleScope
  | DeclareNamespaceScope;

/* abstract */ class ScopeBase<
  +TType: ScopeTypeType,
  +TBlock: ESNode,
  +TUpper: Scope | null,
> {
  /**
   * A unique ID for this instance - primarily used to help debugging and testing
   */
  +$id: number = generator();

  /**
   * The AST node which created this scope.
   * @public
   */
  +block: TBlock;
  /**
   * The array of child scopes. This does not include grandchild scopes.
   * @public
   */
  +childScopes: Array<Scope> = [];
  /**
   * A map of the variables for each node in this scope.
   * This is map is a pointer to the one in the parent ScopeManager instance
   */
  +__declaredVariables: WeakMap<ESNode, Array<Variable>>;
  /**
   * Generally, through the lexical scoping of JS you can always know which variable an identifier in the source code
   * refers to. There are a few exceptions to this rule. With `global` and `with` scopes you can only decide at runtime
   * which variable a reference refers to.
   * All those scopes are considered "dynamic".
   */
  __dynamic: boolean;
  /**
   * Whether this scope is created by a FunctionExpression.
   * @public
   */
  +functionExpressionScope: boolean = false;
  /**
   * Whether 'use strict' is in effect in this scope.
   * @public
   */
  isStrict: boolean;
  /**
   * List of {@link Reference}s that are left to be resolved (i.e. which
   * need to be linked to the variable they refer to).
   */
  __referencesLeftToResolve: Array<Reference> | null = [];
  /**
   * Any variable {@link Reference} found in this scope.
   * This includes occurrences of local variables as well as variables from parent scopes (including the global scope).
   * For local variables this also includes defining occurrences (like in a 'var' statement).
   * In a 'function' scope this does not include the occurrences of the formal parameter in the parameter list.
   * @public
   */
  +references: Array<Reference> = [];
  /**
   * The map from variable names to variable objects.
   * @public
   */
  +set: Map<string, Variable> = new Map<string, Variable>();
  /**
   * The {@link Reference}s that are not resolved with this scope.
   * @public
   */
  +through: Array<Reference> = [];
  /**
   * The type of scope
   * @public
   */
  +type: TType;
  /**
   * Reference to the parent {@link Scope}.
   * @public
   */
  +upper: TUpper;
  /**
   * The scoped {@link Variable}s of this scope.
   * In the case of a 'function' scope this includes the automatic argument `arguments` as its first element, as well
   * as all further formal arguments.
   * This does not include variables which are defined in child scopes.
   * @public
   */
  +variables: Array<Variable> = [];
  /**
   * For scopes that can contain variable declarations, this is a self-reference.
   * For other scope types this is the *variableScope* value of the parent scope.
   * @public
   */
  +variableScope: VariableScope;
  /**
   * The names that are indirectly referenced within this scope.
   * @private
   */
  +__indirectReferences: Set<string> = new Set();

  constructor(
    scopeManager: ScopeManager,
    type: TType,
    upperScope: TUpper,
    block: TBlock,
    isMethodDefinition: boolean,
  ) {
    this.type = type;

    this.__dynamic =
      // $FlowFixMe[invalid-compare]
      this.type === ScopeType.Global || this.type === ScopeType.With;

    this.block = block;

    this.variableScope =
      this.type === ScopeType.ClassFieldInitializer ||
      // $FlowFixMe[invalid-compare]
      this.type === ScopeType.ClassStaticBlock ||
      // $FlowFixMe[invalid-compare]
      this.type === ScopeType.Function ||
      // $FlowFixMe[invalid-compare]
      this.type === ScopeType.Global ||
      // $FlowFixMe[invalid-compare]
      this.type === ScopeType.Module ||
      // $FlowFixMe[invalid-compare]
      this.type === ScopeType.DeclareModule ||
      // $FlowFixMe[invalid-compare]
      this.type === ScopeType.DeclareNamespace
        ? // $FlowFixMe[incompatible-type] not possible to teach flow this is safe
          this
        : // $FlowFixMe[incompatible-use] upperScope can only be null for Global scope
          upperScope.variableScope;

    this.upper = upperScope;

    this.isStrict = isStrictScope(asScope(this), isMethodDefinition);

    if (this.upper) {
      this.upper.childScopes.push(asScope(this));
    }

    this.__declaredVariables = scopeManager.declaredVariables;

    registerScope(scopeManager, asScope(this));
  }

  shouldStaticallyClose(): boolean {
    return !this.__dynamic;
  }

  _shouldStaticallyCloseForGlobal(
    ref: Reference,
    scopeManager: ScopeManager,
  ): boolean {
    // On global scope, let/const/class declarations should be resolved statically.
    const name = ref.identifier.name;

    const variable = this.set.get(name);
    if (!variable) {
      return false;
    }
    // variable exists on the scope

    // in module mode, we can statically resolve everything, regardless of its decl type
    if (scopeManager.isModule()) {
      return true;
    }

    // in script mode, only certain cases should be statically resolved
    // Example:
    // a `var` decl is ignored by the runtime if it clashes with a global name
    // this means that we should not resolve the reference to the variable
    const defs = variable.defs;
    return (
      defs.length > 0 &&
      defs.every(def => {
        if (
          def.type === DefinitionType.Variable &&
          def.parent?.type === 'VariableDeclaration' &&
          def.parent.kind === 'var'
        ) {
          return false;
        }
        return true;
      })
    );
  }

  _staticCloseRef = (ref: Reference, _?: ScopeManager): void => {
    const resolve = (): boolean => {
      const name = ref.identifier.name;
      const variable = this.set.get(name);

      if (!variable) {
        return false;
      }

      if (!this.__isValidResolution(ref, variable)) {
        return false;
      }

      // make sure we don't match a type reference to a value variable
      const isValidTypeReference =
        ref.isTypeReference && variable.isTypeVariable;
      const isValidValueReference =
        ref.isValueReference && variable.isValueVariable;

      if (!isValidTypeReference && !isValidValueReference) {
        return false;
      }

      variable.references.push(ref);
      ref.resolved = variable;

      return true;
    };

    if (!resolve()) {
      this.__delegateToUpperScope(ref);
    }
  };

  _dynamicCloseRef = (ref: Reference, _?: ScopeManager): void => {
    // notify all names are through to global
    let current: Scope | null = asScope(this);

    while (current) {
      current.through.push(ref);
      current = current.upper;
    }
  };

  _globalCloseRef = (ref: Reference, scopeManager: ScopeManager): void => {
    // let/const/class declarations should be resolved statically.
    // others should be resolved dynamically.
    if (this._shouldStaticallyCloseForGlobal(ref, scopeManager)) {
      this._staticCloseRef(ref);
    } else {
      this._dynamicCloseRef(ref);
    }
  };

  close(scopeManager: ScopeManager): Scope | null {
    let closeRef: (Reference, ScopeManager) => void;

    if (this.shouldStaticallyClose()) {
      closeRef = this._staticCloseRef;
    } else if (this.type !== 'global') {
      closeRef = this._dynamicCloseRef;
    } else {
      closeRef = this._globalCloseRef;
    }

    // Try Resolving all references in this scope.
    if (this.__referencesLeftToResolve == null) {
      throw new Error('__referencesLeftToResolve was unexpectedly null.');
    }
    for (let i = 0; i < this.__referencesLeftToResolve.length; ++i) {
      const ref = this.__referencesLeftToResolve[i];
      closeRef(ref, scopeManager);
    }
    this.__referencesLeftToResolve = null;

    if (this.__indirectReferences.size > 0) {
      const upper = this.upper;
      for (const name of this.__indirectReferences) {
        const variable = this.set.get(name);
        if (variable) {
          variable.eslintUsed = true;
          this.__indirectReferences.delete(name);
          continue;
        }
        // delegate it to the upper scope
        if (upper) {
          upper.__indirectReferences.add(name);
          this.__indirectReferences.delete(name);
        }
      }
    }

    return this.upper;
  }

  /**
   * To override by function scopes.
   * References in default parameters isn't resolved to variables which are in their function body.
   */
  __isValidResolution(_ref: Reference, _variable: Variable): boolean {
    return true;
  }

  __delegateToUpperScope(ref: Reference): void {
    if (this.upper?.__referencesLeftToResolve) {
      this.upper.__referencesLeftToResolve.push(ref);
    }
    this.through.push(ref);
  }

  _addDeclaredVariablesOfNode(variable: Variable, node: ?ESNode): void {
    if (node == null) {
      return;
    }

    let variables = this.__declaredVariables.get(node);

    if (variables == null) {
      variables = [];
      this.__declaredVariables.set(node, variables);
    }
    if (!variables.includes(variable)) {
      variables.push(variable);
    }
  }

  __defineVariable(
    nameOrVariable: string | Variable,
    set: Map<string, Variable>,
    variables: Array<Variable>,
    node: Identifier | null,
    def: Definition | null,
  ): void {
    const name =
      typeof nameOrVariable === 'string' ? nameOrVariable : nameOrVariable.name;
    let variable = set.get(name);
    if (!variable) {
      variable =
        typeof nameOrVariable === 'string'
          ? new Variable(name, asScope(this))
          : nameOrVariable;
      set.set(name, variable);
      variables.push(variable);
    }

    if (def) {
      variable.defs.push(def);
      this._addDeclaredVariablesOfNode(variable, def.node);
      this._addDeclaredVariablesOfNode(variable, def.parent);
    }
    if (node) {
      variable.identifiers.push(node);
    }
  }

  defineIdentifier(node: Identifier, def: Definition): void {
    this.__defineVariable(node.name, this.set, this.variables, node, def);
  }

  defineLiteralIdentifier(node: StringLiteral, def: Definition): void {
    this.__defineVariable(node.value, this.set, this.variables, null, def);
  }

  referenceValue(
    node: Identifier | JSXIdentifier,
    assign: ReferenceFlagType = ReferenceFlag.Read,
    writeExpr?: Expression | null,
    maybeImplicitGlobal?: ReferenceImplicitGlobal | null,
    init: boolean = false,
  ): void {
    const ref = new Reference(
      node,
      asScope(this),
      assign,
      writeExpr,
      maybeImplicitGlobal,
      init,
      ReferenceTypeFlag.Value,
    );

    this.references.push(ref);
    this.__referencesLeftToResolve?.push(ref);
  }

  /**
   * Creates an indirect reference to a given `name` `from` the given location
   * This is useful when a build process is expected to create a reference to
   * the name, for example - the JSX transform that references a JSX pragma (React)
   */
  indirectlyReferenceValue(name: string): void {
    this.__indirectReferences.add(name);
  }

  referenceType(node: Identifier): void {
    const ref = new Reference(
      node,
      asScope(this),
      ReferenceFlag.Read,
      null,
      null,
      false,
      ReferenceTypeFlag.Type,
    );

    this.references.push(ref);
    this.__referencesLeftToResolve?.push(ref);
  }

  referenceDualValueType(node: Identifier): void {
    const ref = new Reference(
      node,
      asScope(this),
      ReferenceFlag.Read,
      null,
      null,
      false,
      ReferenceTypeFlag.ValueAndType,
    );

    this.references.push(ref);
    this.__referencesLeftToResolve?.push(ref);
  }
}

export {ScopeBase};
