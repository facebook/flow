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
import type {Scope} from './scope/Scope';

import {
  BlockScope,
  CatchScope,
  ComponentScope,
  HookScope,
  ClassScope,
  DeclareModuleScope,
  DeclareNamespaceScope,
  ForScope,
  FunctionExpressionNameScope,
  FunctionScope,
  GlobalScope,
  MatchCaseScope,
  ModuleScope,
  RecordScope,
  SwitchScope,
  TypeScope,
  WithScope,
} from './scope';
import {ClassFieldInitializerScope} from './scope/ClassFieldInitializerScope';
import {ClassStaticBlockScope} from './scope/ClassStaticBlockScope';

import {Variable} from './variable';
import {ScopeType} from './scope/ScopeType';

type ScopeManagerOptions = $ReadOnly<{
  globalReturn?: boolean,
  sourceType?: 'module' | 'script',
}>;

class ScopeManager {
  currentScope: Scope | null = null;
  +declaredVariables: WeakMap<ESNode, Array<Variable>> = new WeakMap();
  /**
   * The root scope
   * @public
   */
  globalScope: GlobalScope;
  +nodeToScope: WeakMap<ESNode, Array<Scope>> = new WeakMap();
  +_options: ScopeManagerOptions;
  /**
   * All scopes
   * @public
   */
  +scopes: Array<Scope> = [];

  // $FlowExpectedError[unsafe-getters-setters]
  get variables(): Array<Variable> {
    const variables = new Set<Variable>();
    function recurse(scope: Scope): void {
      scope.variables.forEach(v => variables.add(v));
      scope.childScopes.forEach(recurse);
    }
    this.scopes.forEach(recurse);
    return Array.from(variables).sort((a, b) => a.$id - b.$id);
  }

  constructor(options: ScopeManagerOptions) {
    this._options = options;
  }

  isGlobalReturn(): boolean {
    return this._options.globalReturn === true;
  }

  isModule(): boolean {
    return this._options.sourceType === 'module';
  }

  isImpliedStrict(): boolean {
    return this.isModule();
  }
  isStrictModeSupported(): boolean {
    return true;
  }

  isES6(): boolean {
    return true;
  }

  /**
   * Get the variables that a given AST node defines. The gotten variables' `def[].node`/`def[].parent` property is the node.
   * If the node does not define any variable, this returns an empty array.
   * @param node An AST node to get their variables.
   * @public
   */
  getDeclaredVariables(node: ESNode): Array<Variable> {
    return this.declaredVariables.get(node) ?? [];
  }

  /**
   * Get the scope of a given AST node. The gotten scope's `block` property is the node.
   * This method never returns `function-expression-name` scope. If the node does not have their scope, this returns `null`.
   *
   * @param node An AST node to get their scope.
   * @param inner If the node has multiple scopes, this returns the outermost scope normally.
   *                If `inner` is `true` then this returns the innermost scope.
   * @public
   */
  acquire(node: ESNode, inner: boolean = false): Scope | null {
    function predicate(testScope: Scope): boolean {
      if (testScope.type === 'function' && testScope.functionExpressionScope) {
        return false;
      }
      return true;
    }

    const scopes = this.nodeToScope.get(node);

    if (!scopes || scopes.length === 0) {
      return null;
    }

    // Heuristic selection from all scopes.
    // If you would like to get all scopes, please use ScopeManager#acquireAll.
    if (scopes.length === 1) {
      return scopes[0];
    }

    if (inner) {
      for (let i = scopes.length - 1; i >= 0; --i) {
        const scope = scopes[i];

        if (predicate(scope)) {
          return scope;
        }
      }
    } else {
      for (let i = 0; i < scopes.length; ++i) {
        const scope = scopes[i];

        if (predicate(scope)) {
          return scope;
        }
      }
    }

    return null;
  }

  _assertCurrentScope(): Scope {
    if (this.currentScope == null) {
      throw new Error('currentScope was unexpectedly null.');
    }

    return this.currentScope;
  }
  _nestScope<T: Scope>(scope: T): T {
    if (scope instanceof GlobalScope) {
      this.globalScope = scope;
    }
    this.currentScope = scope;
    return scope;
  }

  nestBlockScope(node: BlockScope['block']): BlockScope {
    return this._nestScope(
      new BlockScope(this, this._assertCurrentScope(), node),
    );
  }

  nestCatchScope(node: CatchScope['block']): CatchScope {
    return this._nestScope(
      new CatchScope(this, this._assertCurrentScope(), node),
    );
  }

  nestClassScope(node: ClassScope['block']): ClassScope {
    return this._nestScope(
      new ClassScope(this, this._assertCurrentScope(), node),
    );
  }

  nestClassFieldInitializerScope(
    node: ClassFieldInitializerScope['block'],
  ): ClassFieldInitializerScope {
    const currentScope = this._assertCurrentScope();
    if (currentScope.type !== ScopeType.Class) {
      throw new Error('Expected current scope to be a class scope.');
    }
    return this._nestScope(
      new ClassFieldInitializerScope(this, currentScope, node),
    );
  }

  nestClassStaticBlockScope(
    node: ClassStaticBlockScope['block'],
  ): ClassStaticBlockScope {
    const currentScope = this._assertCurrentScope();
    if (currentScope.type !== ScopeType.Class) {
      throw new Error('Expected current scope to be a class scope.');
    }
    return this._nestScope(new ClassStaticBlockScope(this, currentScope, node));
  }

  nestDeclareModuleScope(
    node: DeclareModuleScope['block'],
  ): DeclareModuleScope {
    return this._nestScope(
      new DeclareModuleScope(this, this._assertCurrentScope(), node),
    );
  }

  nestDeclareNamespaceScope(
    node: DeclareNamespaceScope['block'],
  ): DeclareNamespaceScope {
    return this._nestScope(
      new DeclareNamespaceScope(this, this._assertCurrentScope(), node),
    );
  }

  nestForScope(node: ForScope['block']): ForScope {
    return this._nestScope(
      new ForScope(this, this._assertCurrentScope(), node),
    );
  }

  nestFunctionExpressionNameScope(
    node: FunctionExpressionNameScope['block'],
  ): FunctionExpressionNameScope {
    return this._nestScope(
      new FunctionExpressionNameScope(this, this._assertCurrentScope(), node),
    );
  }

  nestComponentScope(node: ComponentScope['block']): ComponentScope {
    return this._nestScope(
      new ComponentScope(this, this._assertCurrentScope(), node),
    );
  }

  nestHookScope(node: HookScope['block']): HookScope {
    return this._nestScope(
      new HookScope(this, this._assertCurrentScope(), node),
    );
  }

  nestFunctionScope(
    node: FunctionScope['block'],
    isMethodDefinition: boolean,
  ): FunctionScope {
    return this._nestScope(
      new FunctionScope(
        this,
        this._assertCurrentScope(),
        node,
        isMethodDefinition,
      ),
    );
  }

  nestGlobalScope(node: GlobalScope['block']): GlobalScope {
    return this._nestScope(new GlobalScope(this, node));
  }

  nestModuleScope(node: ModuleScope['block']): ModuleScope {
    return this._nestScope(
      new ModuleScope(this, this._assertCurrentScope(), node),
    );
  }

  nestSwitchScope(node: SwitchScope['block']): SwitchScope {
    return this._nestScope(
      new SwitchScope(this, this._assertCurrentScope(), node),
    );
  }

  nestTypeScope(node: TypeScope['block']): TypeScope {
    return this._nestScope(
      new TypeScope(this, this._assertCurrentScope(), node),
    );
  }

  nestWithScope(node: WithScope['block']): WithScope {
    return this._nestScope(
      new WithScope(this, this._assertCurrentScope(), node),
    );
  }

  nestMatchCaseScope(node: MatchCaseScope['block']): MatchCaseScope {
    return this._nestScope(
      new MatchCaseScope(this, this._assertCurrentScope(), node),
    );
  }

  nestRecordScope(node: RecordScope['block']): RecordScope {
    return this._nestScope(
      new RecordScope(this, this._assertCurrentScope(), node),
    );
  }
}

export {ScopeManager};
