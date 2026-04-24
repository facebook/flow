/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import {t, transform} from './test-utils';

function codemod(code: string, useCodeFrame: boolean = true) {
  return transform(code, context => ({
    CallExpression(node) {
      const buildCodeFrame = useCodeFrame
        ? context.buildCodeFrame
        : context.buildSimpleCodeFrame;

      if (
        node.callee.type !== 'MemberExpression' ||
        node.callee.property.type !== 'Identifier' ||
        node.callee.property.name !== 'bind'
      ) {
        // is not a .bind call
        return;
      }
      if (node.callee.object.type !== 'FunctionExpression') {
        // is not binding a function expression
        return;
      }

      if (
        node.arguments.length !== 1 ||
        node.arguments[0]?.type !== 'ThisExpression'
      ) {
        // not bound to `this`
        console.log(
          buildCodeFrame(node.arguments[0] ?? node, 'Not bound to `this`.'),
        );
        return;
      }

      const func = node.callee.object;

      if (func.generator) {
        // arrow functions cannot be generators
        console.log(buildCodeFrame(func, 'Is a generator.'));
        return;
      }

      if (
        func.params.length > 0 &&
        func.params[0].type === 'Identifier' &&
        func.params[0].name === 'this'
      ) {
        // the user has manually typed the `this` context.
        // we don't know if their manual `this` type matches the implicit
        // `this` type - so we can't safely convert it
        console.log(
          buildCodeFrame(func.params[0], 'Manually typed `this` param.'),
        );
        return;
      }

      if (func.id) {
        const funcVar = context.getDeclaredVariables(func);
        if (funcVar.length > 0 && funcVar[0].references.length > 0) {
          // this is a self referencing function expression:
          // ```
          // const x = function y() { y() }.bind(this);
          // ```
          // so we can't convert this into an arrow function
          console.log(buildCodeFrame(func, 'Self-referencing.'));
          return;
        }
      }

      const argumentsVar = context
        .getScope(func.body)
        .variables.find(v => v.name === 'arguments');
      if (argumentsVar && argumentsVar.references.length > 0) {
        // if the function uses `arguments` we can't auto-convert it
        console.log(buildCodeFrame(func, 'Uses `arguments`.'));
        return;
      }

      context.replaceNode(
        node,
        t.ArrowFunctionExpression({
          async: func.async,
          body: func.body,
          params: func.params,
          predicate: func.predicate,
          typeParameters: func.typeParameters,
          returnType: func.returnType,
        }),
      );
    },
  }));
}

describe('React to react', () => {
  it('should transform valid cases correctly', async () => {
    const result = await codemod(`\
a = function y() {
  console.log(this.getMessage());
}.bind(this);

b = function y(arg1: string, arg2: number) {
  console.log(this.getMessage(), arg1, arg2);
}.bind(this);

c = function y<T>(arg1: T) {
  console.log(this.getMessage(), arg1, arg2);
}.bind(this);

d = function y(): void {}.bind(this);
`);

    expect(result).toBe(`\
a = () => {
  console.log(this.getMessage());
};

b = (arg1: string, arg2: number) => {
  console.log(this.getMessage(), arg1, arg2);
};

c = <T>(arg1: T) => {
  console.log(this.getMessage(), arg1, arg2);
};

d = (): void => {};
`);
  });

  beforeEach(() => {
    // $FlowExpectedError[cannot-write]
    console.log = jest.fn();
  });

  const ignoredExamples = `\
a = function y() {
  y();
}.bind(this);

b = function *y() {
  yield true;
}.bind(this);

c = function y(this: Map<string, string>) {
  this.set('a', 'b');
}.bind(this);

d = function y(): void {}.call(this);

e = function y() {
  arguments[0];
}.bind(this);

f = function y() {}.bind(foo);
`;

  it('should ignore invalid cases correctly', async () => {
    const result = await codemod(ignoredExamples);

    expect(result).toBe(`\
a = function y() {
  y();
}.bind(this);

b = function *y() {
  yield true;
}.bind(this);

c = function y(this: Map<string, string>) {
  this.set('a', 'b');
}.bind(this);

d = function y(): void {}.call(this);

e = function y() {
  arguments[0];
}.bind(this);

f = function y() {}.bind(foo);
`);
  });

  it('should log with codeframe when using logWithNode', async () => {
    const logSpy = ((console.log: $FlowFixMe): JestMockFn<Array<mixed>, mixed>);

    await codemod(ignoredExamples, true);

    expect(logSpy).toHaveBeenCalledTimes(5);
    expect(logSpy.mock.calls[0][0]).toMatchInlineSnapshot(`
      "> 1 | a = function y() {
          |     ^^^^^^^^^^^^^^
      > 2 |   y();
          | ^^^^^^
      > 3 | }.bind(this);
          | ^^ Self-referencing."
    `);
    expect(logSpy.mock.calls[1][0]).toMatchInlineSnapshot(`
      "> 5 | b = function *y() {
          |     ^^^^^^^^^^^^^^^
      > 6 |   yield true;
          | ^^^^^^^^^^^^^
      > 7 | }.bind(this);
          | ^^ Is a generator."
    `);
    expect(logSpy.mock.calls[2][0]).toMatchInlineSnapshot(`
      "> 9 | c = function y(this: Map<string, string>) {
          |                ^^^^^^^^^^^^^^^^^^^^^^^^^ Manually typed \`this\` param."
    `);
    expect(logSpy.mock.calls[3][0]).toMatchInlineSnapshot(`
      "> 15 | e = function y() {
           |     ^^^^^^^^^^^^^^
      > 16 |   arguments[0];
           | ^^^^^^^^^^^^^^^
      > 17 | }.bind(this);
           | ^^ Uses \`arguments\`."
    `);
  });

  it('should log with a prefix when using logWithNode', async () => {
    const logSpy = ((console.log: $FlowFixMe): JestMockFn<Array<mixed>, mixed>);

    await codemod(ignoredExamples, false);

    expect(logSpy).toHaveBeenCalledTimes(5);
    expect(logSpy.mock.calls).toMatchInlineSnapshot(`
      [
        [
          "[FunctionExpression:1:4] Self-referencing.",
        ],
        [
          "[FunctionExpression:5:4] Is a generator.",
        ],
        [
          "[Identifier:9:15] Manually typed \`this\` param.",
        ],
        [
          "[FunctionExpression:15:4] Uses \`arguments\`.",
        ],
        [
          "[Identifier:19:25] Not bound to \`this\`.",
        ],
      ]
    `);
  });
});
