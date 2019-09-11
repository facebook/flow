/**
 * @flow
 * @format
 */

import colors from 'colors/safe';
import {format} from 'util';

import {noErrors} from '../flowResult';
import {TestStep, TestStepFirstStage} from './TestStep';
import {newEnv} from './stepEnv';
import {withTimeout, writeFile} from '../utils/async';

import type Builder, {TestBuilder} from './builder';
import type Suite from './Suite';
import type {StepResult} from './TestStep';

type TestResult = {
  name: ?string,
  stepResults: Array<StepResult>,
};

export type SuiteResult =
  | {
      type: 'exceptional',
      message: string,
    }
  | {
      type: 'normal',
      testResults: Array<TestResult>,
    };

export default (async function(
  bin: string,
  builder: Builder,
  suiteName: string,
  testSuite: Suite,
  reportStatus: (status: string, details: string) => void,
): Promise<SuiteResult> {
  const testResults = [];
  let testNum = 1;

  let stepsPassed = 0;
  let stepsFailed = 0;
  let testsRun = 0;
  let totalTests = 0;

  function printStatus(status: 'RUN' | 'PASS' | 'FAIL' | 'ERROR'): void {
    let statusText = colors.grey.bold('[ ] RUN') + ':  ';
    if (status === 'PASS') {
      statusText = colors.green.bold('[\u2713] PASS') + ': '; // checkmark unicode
    } else if (status === 'FAIL') {
      statusText = colors.red.bold('[\u2717] FAIL') + ': '; // x unicode
    } else if (status === 'ERROR') {
      statusText = colors.bgRed(colors.white.bold('[!] ERROR')) + ':';
    }
    reportStatus(
      statusText,
      format(
        colors.grey('(%d/%d tests. %d steps passed. %d steps failed)'),
        testsRun,
        totalTests,
        stepsPassed,
        stepsFailed,
      ),
    );
  }

  const emptyTestStep = new TestStepFirstStage();
  let tests;
  try {
    tests = testSuite.tests(emptyTestStep);
  } catch (e) {
    printStatus('ERROR');
    return {
      type: 'exceptional',
      message: format(
        'Exception while generating test steps by running test.js:\n%s',
        e.stack,
      ),
    };
  }

  totalTests = tests.length;

  for (const test of tests) {
    const steps = [].concat(testSuite.getBeforeEach(emptyTestStep), test.steps);
    const stepResults = [];
    try {
      /* flowErrors contains the current flow errors. If a step doesn't care
       * about flow errors, we won't check for the current flow errors and
       * flowErrors will contain null.
       *
       * In non-lazy mode, we can just assume we start with 0 errors and avoid
       * the initial check. In lazy mode, we must do the initial check
       */
      let flowErrors = test.lazyMode === null ? noErrors : null;

      let testBuilder: TestBuilder = await builder.createFreshTest(
        bin,
        suiteName,
        testNum,
        test.flowConfigFilename,
        test.lazyMode,
        test.shouldWaitForRecheck,
      );

      let firstIdeStartStep = null;
      let lastIdeAssertionStep = null;
      for (let i = 0; i < steps.length; i++) {
        const step = steps[i];
        if (firstIdeStartStep == null && step.startsIde()) {
          firstIdeStartStep = i;
        }
        if (step.readsIdeMessages()) {
          lastIdeAssertionStep = i;
        }
      }

      if (firstIdeStartStep !== null && lastIdeAssertionStep !== null) {
        for (let i = firstIdeStartStep + 1; i <= lastIdeAssertionStep; i++) {
          const step = steps[i];
          if (!step.readsIdeMessages()) {
            throw new Error(
              format(
                "Test '%s' step %d/%d must call either " +
                  'waitAndVerifyAllLSPMessagesContentSinceStartOfStep or waitAndVerifyNoLSPMessagesSinceStartOfStep. ' +
                  'We enforce this as a sanity-check, because testing flow lsp is tricky... ' +
                  'Every step after the first lspStart step until the last lspExpect step ' +
                  'must read LSP messages.\n\n',
                test.name,
                i + 1,
                steps.length,
              ),
            );
          }
        }
      }

      for (let i = 0; i < steps.length; i++) {
        const step = steps[i];
        if (!(step instanceof TestStep)) {
          throw new Error(format('Expected a TestStep, instead got', step));
        }
        printStatus('RUN');
        await testBuilder.log('\nSTEP %d', i + 1);

        if (step.needsFlowServer()) {
          // No-op if one is already running
          await testBuilder.startFlowServer();
        }

        // If one of the assertions will check flow errors, then we need to make
        // sure we have the current errors BEFORE doing any actions
        if (flowErrors == null && step.needsFlowCheck()) {
          flowErrors = await testBuilder.getFlowErrors();
        }
        testBuilder.clearLSPMessages();
        testBuilder.clearLSPStderr();
        let {envRead, envWrite} = newEnv(flowErrors || noErrors);

        testBuilder.setAllowFlowServerToDie(step.allowFlowServerToDie());

        const timeout = step.getTimeout();

        let actions_promise = step.performActions(testBuilder, envWrite);
        if (timeout == null) {
          await actions_promise;
        } else {
          await withTimeout(timeout, actions_promise, () => {
            testBuilder.log('Hit timeout of %dms.', timeout);
          });
        }

        let oldErrors = flowErrors;

        // If any of the actions affect flow and if the assertions check flow
        // errors, then get the new flow errors
        if (envRead.shouldRunFlow() && step.needsFlowCheck()) {
          flowErrors = await testBuilder.getFlowErrors();
          envWrite.setNewErrors(flowErrors);
        } else {
          flowErrors = null;
        }

        envWrite.setLSPMessagesSinceStartOfStep(
          testBuilder.getLSPMessagesSinceStartOfStep(),
        );
        envWrite.setLSPStderrSinceStartOfStep(
          testBuilder.getLSPStderrSinceStartOfStep(),
        );
        envWrite.setServerRunning(
          testBuilder.server == null ? 'stopped' : 'running',
        );
        envWrite.setLSPRunning(testBuilder.lsp == null ? 'stopped' : 'running');

        let result = step.checkAssertions(envRead);
        testBuilder.assertNoErrors();
        testBuilder.setAllowFlowServerToDie(false);
        if (result.passed) {
          stepsPassed++;
        } else {
          stepsFailed++;
        }
        stepResults.push(result);
      }

      await testBuilder.cleanup();
    } catch (e) {
      printStatus('ERROR');
      return {
        type: 'exceptional',
        message: format('Exception while running test steps:\n%s', e.stack),
      };
    }
    testsRun++;
    testResults.push({
      name: test.name,
      stepResults,
    });
    testNum++;
  }
  if (stepsFailed === 0) {
    printStatus('PASS');
  } else {
    printStatus('FAIL');
  }

  return {
    type: 'normal',
    testResults,
  };
});
