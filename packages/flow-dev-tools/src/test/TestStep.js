/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import Builder from './builder';
import searchStackForTestAssertion from './searchStackForTestAssertion';
import newErrors from './assertions/newErrors';
import noNewErrors from './assertions/noNewErrors';
import stderr from './assertions/stderr';
import stdout from './assertions/stdout';
import sortedStdout from './assertions/sortedStdout';
import exitCodes from './assertions/exitCodes';
import serverRunning from './assertions/serverRunning';
import noop from './assertions/noop';
import lspNoNewMessagesAfterSleep from './assertions/lspNoNewMessagesAfterSleep';
import lspNewMessagesWithTimeout from './assertions/lspNewMessagesWithTimeout';
import lspStderr from './assertions/lspStderr';
import simpleDiffAssertion from './assertions/simpleDiffAssertion';

import {sleep} from '../utils/async';

import type {
  AssertionLocation,
  ErrorAssertion,
  ErrorAssertionResult,
  Suggestion,
} from './assertions/assertionTypes';
import type {TestBuilder} from './builder';
import type {FlowResult} from '../flowResult';
import type {StepEnvReadable, StepEnvWriteable} from './stepEnv';
import type {LSPMessage} from './lsp';

type Action = (
  builder: TestBuilder,
  envWrite: StepEnvWriteable,
) => Promise<void>;

export type StepResult = {
  passed: boolean,
  assertionResults: Array<ErrorAssertionResult>,
  exception?: Error,
};

/**
 * A test suite is made up of tests. A test is made up of test steps. When you
 * write a test you create these test steps.
 *
 * Each test step is made up of
 * A) 0 or more actions
 * B) 0 or more assertions
 * C) If it has at least 1 assertion, then 0 or 1 reason.
 *
 * When the test step is run by the test runner, it will perform all the
 * actions and then perform all the assertions
 *
 * I tried to be a little clever with how I structured these classes. The goal
 * is to make writing tests easy. I limited which methods are available by
 * splitting the TestStep class into 3 classes representing the 3 stages.
 *
 * Stage 1: Specifying actions
 * Stage 2: Specifying assertions
 * Stage 3: Specifying the reason
 *
 * I also made the TestSteps immutable-ish so that when you call a method it
 * just makes a copy of the step. Each test suite will be given a stage 1
 * TestStep with 0 actions and 0 assertions.
 */
export class TestStep {
  _actions: Array<Action>;
  _assertions: Array<ErrorAssertion>;
  _reason: ?string;
  _needsFlowServer: boolean; // makes runTestSuite start flow server before executing step's action
  _needsFlowCheck: boolean;
  _startsIde: boolean;
  _readsIdeMessages: boolean;
  _allowServerToDie: boolean;
  _timeout: ?number;

  constructor(step?: TestStep) {
    this._actions = step == null ? [] : step._actions.slice();
    this._assertions = step == null ? [] : step._assertions.slice();
    this._needsFlowCheck = step == null ? false : step._needsFlowCheck;
    this._needsFlowServer = step == null ? false : step._needsFlowServer;
    this._reason = step == null ? null : step._reason;
    this._startsIde = step == null ? false : step._startsIde;
    this._readsIdeMessages = step == null ? false : step._readsIdeMessages;
    this._allowServerToDie = step == null ? false : step._allowServerToDie;
    this._timeout = step == null ? null : step._timeout;
  }

  async performActions(
    builder: TestBuilder,
    env: StepEnvWriteable,
  ): Promise<void> {
    for (const action of this._actions) {
      await action(builder, env);
    }
  }

  checkAssertions(env: StepEnvReadable): StepResult {
    let passed = true;
    const assertionResults = this._assertions.map(assertion => {
      const result = assertion(this._reason, env);
      passed = passed && result.type == 'pass';
      return result;
    });
    return {passed, assertionResults};
  }

  needsFlowServer(): boolean {
    return this._needsFlowServer;
  }

  needsFlowCheck(): boolean {
    return this._needsFlowCheck;
  }

  startsIde(): boolean {
    return this._startsIde;
  }

  readsIdeMessages(): boolean {
    return this._readsIdeMessages;
  }

  allowFlowServerToDie(): boolean {
    return this._allowServerToDie;
  }

  getTimeout(): ?number {
    return this._timeout;
  }
}

class TestStepFirstOrSecondStage extends TestStep {
  noNewErrors(): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    const ret = this._cloneWithAssertion(noNewErrors(assertLoc));
    ret._needsFlowCheck = true;
    return ret;
  }

  newErrors(expected: string): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    const ret = this._cloneWithAssertion(newErrors(expected, assertLoc));
    ret._needsFlowCheck = true;
    return ret;
  }

  stderr(expected: string): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    return this._cloneWithAssertion(stderr(expected, assertLoc));
  }

  stdout(expected: string): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    return this._cloneWithAssertion(stdout(expected, assertLoc));
  }

  sortedStdout(expected: string): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    return this._cloneWithAssertion(sortedStdout(expected, assertLoc));
  }

  exitCodes(expected: Array<number>): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    return this._cloneWithAssertion(exitCodes(expected, assertLoc));
  }

  verifyServerStatus(expected: 'stopped' | 'running'): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    return this._cloneWithAssertion(serverRunning(expected, assertLoc));
  }

  /* This is mainly useful for debugging. Actual tests probably shouldn't
   * test the stderr output. But when you're working on `flow lsp`, you can
   * log things to stderr and use this assertion to see what's being logged
   *
   *   addCode('foo')
   *     .lspNoNewMessagesAfterSleep(500)
   *     .lspStderr('Foo')
   */
  lspStderr(expected: string): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    const ret = this._cloneWithAssertion(lspStderr(expected, assertLoc));
    ret._needsFlowCheck = true;
    return ret;
  }

  timeout(seconds: number): TestStepSecondStage {
    const ret = new TestStepSecondStage(this);
    ret._timeout = seconds;
    return ret;
  }

  _cloneWithAssertion(assertion: ErrorAssertion): TestStepSecondStage {
    const ret = new TestStepSecondStage(this);
    ret._assertions.push(assertion);
    return ret;
  }
}

/**
 * The various actions are written with arrow functions so that they
 * automatically capture this. This allows us to write tests with destructuring
 * like
 *
 * suite(({addCode}) => [
 *   test([
 *     addCode('foo')
 *     .noErrors(),
 *   ]),
 * ]);
 */
export class TestStepFirstStage extends TestStepFirstOrSecondStage {
  addCode: (code: string) => TestStepFirstStage = code =>
    this._cloneWithAction(async (builder, env) => {
      await builder.addCode(code);
      env.triggerFlowCheck();
    });

  addFile: (source: string, dest?: string) => TestStepFirstStage = (
    source,
    dest,
  ) =>
    this._cloneWithAction(async (builder, env) => {
      await builder.addFile(source, dest || source);
      env.triggerFlowCheck();
    });

  addFiles: (...sources: Array<string>) => TestStepFirstStage = (...sources) =>
    this._cloneWithAction(async (builder, env) => {
      await builder.addFiles(sources);
      env.triggerFlowCheck();
    });

  removeFile: (filename: string) => TestStepFirstStage = filename =>
    this._cloneWithAction(async (builder, env) => {
      await builder.removeFile(filename);
      env.triggerFlowCheck();
    });

  removeFiles: (...filenames: Array<string>) => TestStepFirstStage = (
    ...filenames
  ) =>
    this._cloneWithAction(async (builder, env) => {
      await builder.removeFiles(filenames);
      env.triggerFlowCheck();
    });

  modifyFile: (
    string,
    string | RegExp,
    string,
    ?{triggerFlowCheck: boolean},
  ) => TestStepFirstStage = (filename, searchValue, replaceValue, options) => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.modifyFile(filename, searchValue, replaceValue);
      if (options != null && Boolean(options.triggerFlowCheck)) {
        env.triggerFlowCheck();
      }
    });
    return ret;
  };

  mockShellCommand: string => TestStepFirstStage = name => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.mockShellCommand(name);
    });
    return ret;
  };

  waitUntilMockInvocation: number => TestStepFirstStage = timeout => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.waitUntilMockInvocation(timeout);
    });
    return ret;
  };

  waitUntilMocksSettle: number => TestStepFirstStage = timeout => {
    const ret = this._cloneWithAction(async (builder, env) => {
      while (await builder.waitUntilMockInvocation(timeout));
    });
    return ret;
  };

  verifyMockInvocationsSinceStartOfStepContaining: (
    string,
    string,
    number,
  ) => TestStepSecondStage = (name, substring, expectedCount) => {
    const assertLoc = searchStackForTestAssertion();
    const ret = this._cloneWithAssertion((reason, env) => {
      const actualInvocations = env.getMockInvocationsSinceStartOfStep()[name];
      const actualContaining = actualInvocations.filter(invocation =>
        invocation.some(arg => arg.includes(substring)),
      );
      const actualCount = actualContaining.length;
      const suggestion = {
        method: 'verifyMockInvocationsSinceStartOfStepContaining',
        args: [name, substring, actualCount],
      };
      return simpleDiffAssertion(
        expectedCount.toString(),
        actualCount.toString(),
        assertLoc,
        reason,
        `${name} invocations`,
        suggestion,
      );
    });
    return ret;
  };

  waitUntilLSPStatus: (number, 'stopped' | 'running') => TestStepFirstStage = (
    timeoutMs,
    expected,
  ) => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.waitUntilLSPStatus(timeoutMs, expected);
    });
    return ret;
  };

  verifyLSPStatus: (
    'stopped' | 'running',
  ) => TestStepSecondStage = expected => {
    const assertLoc = searchStackForTestAssertion();
    const ret = this._cloneWithAssertion((reason, env) => {
      const actual = env.getLSPRunning();
      const suggestion = {method: 'verifyLSPStatus', args: [actual]};
      return simpleDiffAssertion(
        expected,
        actual,
        assertLoc,
        reason,
        "'is LSP running?'",
        suggestion,
      );
    });
    return ret;
  };

  waitUntilServerStatus: (
    number,
    'stopped' | 'running',
  ) => TestStepFirstStage = (timeoutMs, expected) => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.waitUntilServerStatus(timeoutMs, expected);
    });
    ret._allowServerToDie = expected === 'stopped';
    return ret;
  };

  lspIgnoreStatusAndCancellation: Array<string> = [
    'window/showStatus',
    '$/cancelRequest',
  ];

  lspInitializeParams: any = {
    rootUri: '<PLACEHOLDER_PROJECT_URL>',
    capabilities: {
      workspace: {},
      textDocument: {
        synchronization: {},
        completion: {},
        hover: {},
        definition: {},
        signatureHelp: {},
        codeAction: {
          codeActionLiteralSupport: {
            codeActionKind: {
              valueSet: ['quickfix', 'refactor.extract'],
            },
          },
        },
      },
      window: {status: {}, progress: {}, actionRequired: {}},
      telemetry: {connectionStatus: {}},
    },
    trace: 'verbose',
  };

  startFlowServer: () => TestStepFirstStage = () => {
    const ret = this._cloneWithAction(async (builder, env) => {});
    ret._needsFlowServer = true;
    return ret;
  };

  lspStart: ({|needsFlowServer: boolean|}) => TestStepFirstStage = arg => {
    const needsFlowServer = arg.needsFlowServer;

    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.createLSPConnection();
    });
    ret._startsIde = true;
    ret._needsFlowServer = needsFlowServer; // to start flow server before action is executed
    return ret;
  };

  lspStartAndConnect: (?number, ?{}) => TestStepSecondStage = (
    timeoutMsOpt,
    initParamsOpt,
  ) => {
    const assertLoc = searchStackForTestAssertion();
    const timeoutMs = timeoutMsOpt || 60000;
    const initParams = initParamsOpt || this.lspInitializeParams;

    const expectedMethod = 'telemetry/connectionStatus';
    const expectedContents = '{true}';
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.createLSPConnection();
      const promise = builder.sendLSPRequestAndWaitForResponse('initialize', [
        initParams,
      ]); // discarding the promise; instead we wait in the next statement...
      await builder.waitUntilLSPMessage(
        timeoutMs,
        expectedMethod,
        expectedContents,
      );
    })._cloneWithAssertion((reason, env) => {
      const isConnected = env
        .getLSPMessagesSinceStartOfStep()
        .some(msg =>
          Builder.doesMessageFuzzyMatch(msg, expectedMethod, expectedContents),
        );
      const suggestion = {
        method: 'lspStartAndConnect',
        args: [timeoutMs * 2],
      };
      return simpleDiffAssertion(
        'connected',
        isConnected
          ? 'connected'
          : 'disconnected' +
              JSON.stringify(env.getLSPMessagesSinceStartOfStep()),
        assertLoc,
        reason,
        "'is connected to flow server?'",
        suggestion,
      );
    });
    ret._startsIde = true;
    ret._needsFlowServer = true;
    return ret;
  };

  lspStop: () => TestStepFirstStage = () => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.cleanupLSPConnection();
    });
    ret._needsFlowServer = true;
    return ret;
  };

  lspNotification: (string, ...params: Array<mixed>) => TestStepFirstStage = (
    method,
    ...params
  ) => {
    const ret = this._cloneWithAction(async (builder, env) =>
      builder.sendLSPNotification(method, params),
    );
    return ret;
  };

  lspResponse: (
    number | 'mostRecent',
    ...params: Array<mixed>
  ) => TestStepFirstStage = (id, ...params) => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.sendLSPResponse(id, params);
    });
    return ret;
  };

  lspRequest: (string, ...params: Array<mixed>) => TestStepFirstStage = (
    method,
    ...params
  ) => {
    const ret = this._cloneWithAction(async (builder, env) => {
      const promise = builder.sendLSPRequestAndWaitForResponse(method, params);
      // We don't do anything with that promise; user will wait for messages later.
      // TODO(ljw): at end of step, verify that no promises are left outstanding
    });
    return ret;
  };

  lspRequestAndWaitUntilResponse: (
    string,
    ...params: Array<mixed>
  ) => TestStepFirstStage = (method, ...params) => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.sendLSPRequestAndWaitForResponse(method, params);
    });
    ret._readsIdeMessages = true;
    return ret;
  };

  waitUntilLSPMessage: (
    timeout: number,
    expectedMethod: string,
    expectedContents?: string,
  ) => TestStepFirstStage = (timeoutMs, method, contents) => {
    const ret = this._cloneWithAction(
      async (builder, env) =>
        await builder.waitUntilLSPMessage(timeoutMs, method, contents),
    );
    ret._readsIdeMessages = true;
    return ret;
  };

  // verifyAllLSPMessagesInStep(expects, ignores) will look at all the actual
  // messages that arrived in this step.
  //
  // For example, if expects = ['A',['B',{a:'b',c:false}]] and
  // ignores = ['B','E'], there must be an "A", with any contents, followed by
  // a "B" which exactly matches the given JSON object.
  //
  // Instead of an object, legacy callers also pass a string containing chunks
  // of JSON which is very loosely compared. the string '{"a":"b","c":false}'
  // tests whether response "B" contains the substrings `"a":"b"` and
  // `"c":false` anywhere in its JSON representation. This doesn't enforce
  // ordering or any structure, so should probably be avoided.
  //
  // It's okay if there are unexpected messages so long as they're in
  // ignores list - in this case we'd ignore any "B" (either because it came
  // in the wrong order or because it didn't have the right contents), and
  // ignore any "E". But if there are unexpected messages not in the ignore
  // list, then we fail.
  verifyAllLSPMessagesInStep: (
    $ReadOnlyArray<string | [string, string] | LSPMessage>,
    $ReadOnlyArray<string | [string, string] | LSPMessage>,
  ) => TestStepSecondStage = (expects, ignores) => {
    const assertLoc = searchStackForTestAssertion();
    const ret = this._cloneWithAssertion((reason, env) => {
      const actualMessages = env.getLSPMessagesSinceStartOfStep();
      let actuals: Array<string | [string, string] | LSPMessage> = [];
      let iExpect = 0;
      // we test that messages are equal using a diff of strings, so we have
      // to convert the expected value into a string.
      let diffable = expected => {
        if (typeof expected === 'string') {
          return expected;
        } else if (Array.isArray(expected)) {
          return expected.join(',');
        } else {
          return JSON.stringify(expected, null, 2);
        }
      };
      let doesMatch = (
        actual: LSPMessage,
        expected: string | [string, string] | LSPMessage,
      ) => {
        if (typeof expected === 'string') {
          return Builder.doesMessageFuzzyMatch(actual, expected);
        } else if (Array.isArray(expected) && expected.length === 2) {
          return Builder.doesMessageFuzzyMatch(
            actual,
            expected[0],
            expected[1],
          );
        } else {
          return Builder.doesMessageMatch(actual, expected);
        }
      };
      for (let iActual = 0; iActual < actualMessages.length; iActual++) {
        let actual = actualMessages[iActual];
        let expected = expects[iExpect];
        if (expected !== undefined && doesMatch(actual, expected)) {
          // it matches (possibly fuzzily), so we add the *expected* output
          // to *actuals*, so that when we string diff it later, it matches.
          // otherwise, the fuzzy expectation wouldn't match later.
          actuals.push(expected);
          iExpect++;
        } else if (ignores.some(ignore => doesMatch(actual, ignore))) {
          // ignore it
        } else {
          actuals.push(actual);
        }
      }

      const suggestion = {
        method: 'verifyAllLSPMessagesInStep',
        args: [actuals, ignores],
      };

      // don't ignore whitespace, since we used JSON.stringify to format
      // both `expects` and `actuals`, whitespace is already normalized
      // and differences are relevant (like within strings).
      const ignoreWhitespace = false;

      return simpleDiffAssertion(
        expects.map(diffable).join('\n'),
        actuals.map(diffable).join('\n'),
        assertLoc,
        reason,
        'messages',
        suggestion,
        ignoreWhitespace,
      );
    });
    ret._readsIdeMessages = true;
    return ret;
  };

  // waitAndVerifyNoLSPMessagesSinceStartOfStep: if any messages arrive since the start
  // of this step until the timeout then it fails; otherwise it succeeds
  waitAndVerifyNoLSPMessagesSinceStartOfStep: number => TestStepSecondStage = timeoutMs => {
    const assertLoc = searchStackForTestAssertion();

    const ret = this._cloneWithAction(async (builder, env) => {
      await sleep(timeoutMs);
    })._cloneWithAssertion(lspNoNewMessagesAfterSleep(timeoutMs, assertLoc));
    ret._readsIdeMessages = true;
    return ret;
  };

  sleep: number => TestStepFirstStage = timeoutMs =>
    this._cloneWithAction(async (builder, env) => {
      await sleep(timeoutMs);
    });

  // waitAndVerifyAllLSPMessagesContentSinceStartOfStep: will consider all messages that
  // have arrived since the start of the step, and will consider further
  // messages that arrive up to the expected message count until the timeout.
  // (This set of messages to consider may therefore be larger than, same
  // size, or smaller than the expected count). If the messages to consider
  // are identical to the expected messages, then it succeeds.
  waitAndVerifyAllLSPMessagesContentSinceStartOfStep: (
    number,
    $ReadOnlyArray<LSPMessage>,
  ) => TestStepSecondStage = (timeoutMs, expected) => {
    const assertLoc = searchStackForTestAssertion();

    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.waitUntilLSPMessageCount(timeoutMs, expected.length);
    })._cloneWithAssertion(
      lspNewMessagesWithTimeout(timeoutMs, expected, assertLoc),
    );
    ret._readsIdeMessages = true;
    return ret;
  };

  flowCmd: (args: Array<string>, stdinFile?: string) => TestStepFirstStage = (
    args,
    stdinFile,
  ) => {
    // Certain flow configs don't need a flow server to exist
    let needsFlowServer = false;
    switch (args[0]) {
      case 'ast':
      case 'init':
      case 'ls':
      case 'start':
      case 'stop':
      case 'version':
        break;
      default:
        needsFlowServer = true;
    }
    if (needsFlowServer) {
      // We never want a flowCmd to automatically start a server
      args = [args[0], '--no-auto-start', ...args.slice(1)];
    }
    const ret = this._cloneWithAction(async (builder, env) => {
      const [code, stdout, stderr] = await builder.flowCmd(args, stdinFile);
      env.reportExitCode(code);
      env.reportStdout(stdout);
      env.reportStderr(stderr);
      env.triggerFlowCheck();
    });

    if (needsFlowServer) {
      ret._needsFlowServer = needsFlowServer;
    }
    return ret;
  };

  dontMindServerDeath: () => TestStepFirstStage = () => {
    const ret = this._cloneWithAction(async (builder, env) => {});
    ret._allowServerToDie = true;
    return ret;
  };

  _cloneWithAction(action: Action): TestStepFirstStage {
    const ret = new TestStepFirstStage(this);
    ret._actions.push(action);
    return ret;
  }
}

class TestStepSecondStage extends TestStepFirstOrSecondStage {
  because(reason: string): TestStepThirdStage {
    return this._cloneWithReason(reason);
  }

  _cloneWithReason(reason: string): TestStepThirdStage {
    const ret = new TestStepThirdStage(this);
    ret._reason = reason;
    return ret;
  }
}

class TestStepThirdStage extends TestStep {}
