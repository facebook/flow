/**
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import searchStackForTestAssertion from './searchStackForTestAssertion';
import newErrors from './assertions/newErrors';
import noNewErrors from './assertions/noNewErrors';
import stderr from './assertions/stderr';
import stdout from './assertions/stdout';
import sortedStdout from './assertions/sortedStdout';
import exitCodes from './assertions/exitCodes';
import serverRunning from './assertions/serverRunning';
import noop from './assertions/noop';
import ideNoNewMessagesAfterSleep from './assertions/ideNoNewMessagesAfterSleep';
import ideNewMessagesWithTimeout from './assertions/ideNewMessagesWithTimeout';
import ideStderr from './assertions/ideStderr';
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
import type {IDEMessage} from './ide';

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

  constructor(step?: TestStep) {
    this._actions = step == null ? [] : step._actions.slice();
    this._assertions = step == null ? [] : step._assertions.slice();
    this._needsFlowCheck = step == null ? false : step._needsFlowCheck;
    this._needsFlowServer = step == null ? false : step._needsFlowServer;
    this._reason = step == null ? null : step._reason;
    this._startsIde = step == null ? false : step._startsIde;
    this._readsIdeMessages = step == null ? false : step._readsIdeMessages;
    this._allowServerToDie = step == null ? false : step._allowServerToDie;
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
   * test the stderr output. But when you're working on `flow ide`, you can
   * log things to stderr and use this assertion to see what's being logged
   *
   *   addCode('foo')
   *     .ideNoNewMessagesAfterSleep(500)
   *     .ideStderr('Foo')
   */
  ideStderr(expected: string): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    const ret = this._cloneWithAssertion(ideStderr(expected, assertLoc));
    ret._needsFlowCheck = true;
    return ret;
  }

  _cloneWithAssertion(assertion: ErrorAssertion) {
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

  waitUntilIDEStatus: (number, 'stopped' | 'running') => TestStepFirstStage = (
    timeoutMs,
    expected,
  ) => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.waitUntilIDEStatus(timeoutMs, expected);
    });
    return ret;
  };

  verifyIDEStatus: (
    'stopped' | 'running',
  ) => TestStepSecondStage = expected => {
    const assertLoc = searchStackForTestAssertion();
    const ret = this._cloneWithAssertion((reason, env) => {
      const actual = env.getIDERunning();
      const suggestion = {method: 'verifyIDEStatus', args: [actual]};
      return simpleDiffAssertion(
        expected,
        actual,
        assertLoc,
        reason,
        "'is IDE running?'",
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
    return ret;
  };

  ideStart: (

      | {|mode: 'legacy'|}
      | {|mode: 'lsp', needsFlowServer: boolean, doInitialize: boolean|},
  ) => TestStepFirstStage = arg => {
    const mode = arg.mode;
    const needsFlowServer = arg.mode === 'legacy' ? true : arg.needsFlowServer;
    const doFlowCheck = arg.mode === 'legacy' ? true : false;
    const doInitialize = arg.mode === 'legacy' ? false : arg.doInitialize;

    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.createIDEConnection(mode);
      if (doFlowCheck) {
        env.triggerFlowCheck();
      }
    });
    ret._startsIde = true;
    ret._needsFlowServer = needsFlowServer; // to start flow server before action is executed
    return ret;
  };

  ideStop: () => TestStepFirstStage = () => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.cleanupIDEConnection();
    });
    ret._needsFlowServer = true;
    return ret;
  };

  ideNotification: (string, ...params: Array<mixed>) => TestStepFirstStage = (
    method,
    ...params
  ) => {
    const ret = this._cloneWithAction(async (builder, env) =>
      builder.sendIDENotification(method, params),
    );
    return ret;
  };

  ideRequest: (string, ...params: Array<mixed>) => TestStepFirstStage = (
    method,
    ...params
  ) => {
    const ret = this._cloneWithAction(async (builder, env) => {
      const promise = builder.sendIDERequestAndWaitForResponse(method, params);
      // We don't do anything with that promise; user will wait for messages later.
      // TODO(ljw): at end of step, verify that no promises are left outstanding
    });
    return ret;
  };

  ideRequestAndWaitUntilResponse: (
    string,
    ...params: Array<mixed>
  ) => TestStepFirstStage = (method, ...params) => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.sendIDERequestAndWaitForResponse(method, params);
    });
    return ret;
  };

  waitUntilIDEMessage: (number, string) => TestStepFirstStage = (
    timeoutMs,
    method,
  ) => {
    const ret = this._cloneWithAction(
      async (builder, env) =>
        await builder.waitUntilIDEMessage(timeoutMs, method),
    );
    ret._readsIdeMessages = true;
    return ret;
  };

  verifyAllIDEMessagesInStep: (
    Array<string>,
    Array<string>,
  ) => TestStepSecondStage = (requiredSequence, ignored) => {
    const assertLoc = searchStackForTestAssertion();
    const ret = this._cloneWithAssertion((reason, env) => {
      const methods = env
        .getIDEMessagesSinceStartOfStep()
        .filter(msg => !ignored.includes(msg.method))
        .map(msg => msg.method);
      const suggestion = {
        method: 'verifyAllIDEMessagesInStep',
        args: ['<FIGURE IT OUT>'],
      };
      return simpleDiffAssertion(
        requiredSequence.join(','),
        methods.join(','),
        assertLoc,
        reason,
        "'what required message arrived'",
        suggestion,
      );
    });
    return ret;
  };

  // waitAndVerifyNoIDEMessagesSinceStartOfStep: if any messages arrive since the start
  // of this step until the timeout then it fails; otherwise it succeeds
  waitAndVerifyNoIDEMessagesSinceStartOfStep: number => TestStepSecondStage = timeoutMs => {
    const assertLoc = searchStackForTestAssertion();

    const ret = this._cloneWithAction(async (builder, env) => {
      await sleep(timeoutMs);
    })._cloneWithAssertion(ideNoNewMessagesAfterSleep(timeoutMs, assertLoc));
    ret._readsIdeMessages = true;
    return ret;
  };

  sleep: number => TestStepFirstStage = timeoutMs =>
    this._cloneWithAction(async (builder, env) => {
      await sleep(timeoutMs);
    });

  // waitAndVerifyAllIDEMessagesContentSinceStartOfStep: will consider all messages that
  // have arrived since the start of the step, and will consider further
  // messages that arrive up to the expected message count until the timeout.
  // (This set of messages to consider may therefore be larger than, same
  // size, or smaller than the expected count). If the messages to consider
  // are identical to the expected messages, then it succeeds.
  waitAndVerifyAllIDEMessagesContentSinceStartOfStep: (
    number,
    $ReadOnlyArray<IDEMessage>,
  ) => TestStepSecondStage = (timeoutMs, expected) => {
    const assertLoc = searchStackForTestAssertion();

    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.waitUntilIDEMessageCount(timeoutMs, expected.length);
    })._cloneWithAssertion(
      ideNewMessagesWithTimeout(timeoutMs, expected, assertLoc),
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

  waitForServerToDie: (timeout: number) => TestStepFirstStage = timeout => {
    const ret = this._cloneWithAction(async (builder, env) => {
      await builder.waitForServerToDie(timeout);
    });
    ret._needsFlowServer = true;
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
