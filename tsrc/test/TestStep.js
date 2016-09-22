/* @flow */

import searchStackForTestAssertion from './searchStackForTestAssertion';
import newErrors from './assertions/newErrors';
import noNewErrors from './assertions/noNewErrors';
import stderr from './assertions/stderr';
import stdout from './assertions/stdout';
import exitCodes from './assertions/exitCodes';
import serverRunning from './assertions/serverRunning';
import noop from './assertions/noop';

import type {
  AssertionLocation,
  ErrorAssertion,
  ErrorAssertionResult,
} from './assertions/assertionTypes';
import type {TestBuilder} from './builder';
import type {FlowResult} from '../flowResult';
import type {StepEnvReadable, StepEnvWriteable} from './stepEnv';

type Action =
  (builder: TestBuilder, envWrite: StepEnvWriteable) => Promise<void>;

export type StepResult = {
  passed: boolean,
  assertionResults: Array<ErrorAssertionResult>,
  exception?: Error,
}

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
  _needsFlowServer: boolean;
  _needsFlowCheck: boolean;

  constructor(step?: TestStep) {
    this._actions = step == null ? [] : step._actions.slice();
    this._assertions = step == null ? [] : step._assertions.slice();
    this._needsFlowCheck = step == null ? false : step._needsFlowCheck;
    this._needsFlowServer = step == null ? false : step._needsFlowServer;
    this._reason = step == null ? null : step._reason;
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
      const result = assertion(this._reason, env)
      passed = passed && result.type == "pass";
      return result;
    });
    return { passed, assertionResults };
  }

  needsFlowServer(): boolean {
    return this._needsFlowServer;
  }

  needsFlowCheck(): boolean {
    return this._needsFlowCheck;
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

  exitCodes(expected: Array<number>): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    return this._cloneWithAssertion(exitCodes(expected, assertLoc));
  }

  serverRunning(expected: boolean): TestStepSecondStage {
    const assertLoc = searchStackForTestAssertion();
    return this._cloneWithAssertion(serverRunning(expected, assertLoc));
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
  addCode: (code:string) => TestStepFirstStage =
    (code) => this._cloneWithAction(
      async (builder, env) => {
        await builder.addCode(code);
        env.triggerFlowCheck();
      }
    );

  addFile: (source: string, dest?: string) => TestStepFirstStage =
    (source, dest) => this._cloneWithAction(
      async (builder, env) => {
        await builder.addFile(source, dest || source);
        env.triggerFlowCheck();
      }
    );

  addFiles: (...sources: Array<string>) => TestStepFirstStage =
    (...sources) => this._cloneWithAction(
      async (builder, env) => {
        await builder.addFiles(sources);
        env.triggerFlowCheck();
      }
    );

  removeFile: (filename: string) => TestStepFirstStage =
    (filename) => this._cloneWithAction(
      async (builder, env) => {
        await builder.removeFile(filename);
        env.triggerFlowCheck();
      }
    );

  removeFiles: (...filenames: Array<string>) => TestStepFirstStage =
    (...filenames) => this._cloneWithAction(
      async (builder, env) => {
        await builder.removeFiles(filenames);
        env.triggerFlowCheck();
      }
    );

  flowCmd: (args: Array<string>, stdinFile?: string) => TestStepFirstStage =
    (args, stdinFile) => {
      const ret = this._cloneWithAction(
        async (builder, env) => {
          const [code, stdout, stderr] = await builder.flowCmd(args, stdinFile);
          env.reportExitCode(code);
          env.reportStdout(stdout);
          env.reportStderr(stderr);
          env.triggerFlowCheck();
        }
      );
      ret._needsFlowServer = true;
      return ret;
    };

  waitForServerToDie: (timeout: number) => TestStepFirstStage =
    (timeout) => {
      const ret = this._cloneWithAction(
        async (builder, env) => {
          await builder.waitForServerToDie(timeout);
        }
      );
      ret._needsFlowServer = true;
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

class TestStepThirdStage extends TestStep {
}
