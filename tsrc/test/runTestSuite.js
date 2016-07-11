/* @flow */

import colors from 'colors/safe';
import {format} from 'util';

import {noErrors} from '../flowResult';
import {TestStep, TestStepFirstStage} from './TestStep';
import {newEnv} from './stepEnv';
import {writeFile} from '../async';

import type Builder from './builder';
import type Suite from './Suite';
import type {StepResult} from './TestStep';

export type TestResult = {
  name: ?string,
  stepResults: Array<StepResult>,
};

export default async function(
  bin: string,
  builder: Builder,
  suiteName: string,
  testSuite: Suite,
  reportStatus: (status: string, details: string) => void,
): Promise<Array<TestResult>> {
  let results = [];
  let testNum = 1;

  const emptyTestStep = new TestStepFirstStage();
  const tests = testSuite.tests(emptyTestStep);

  let stepsPassed = 0;
  let stepsFailed = 0;
  let testsRun = 0;
  const totalTests = tests.length;

  function printStatus(status: 'RUN' | 'PASS' | 'FAIL'): void {
    let statusText = colors.grey.bold("[ ] RUN")+": ";
    let newline = "";
    if (status === 'PASS') {
      statusText = colors.green.bold("[✓] PASS")+":"
      newline = "\n";
    } else if (status === 'FAIL') {
      statusText = colors.red.bold("[✗] FAIL")+":"
      newline = "\n";
    }
    reportStatus(
      statusText,
      format(
        colors.grey("(%d/%d tests. %d steps passed. %d steps failed)"),
        testsRun,
        totalTests,
        stepsPassed,
        stepsFailed,
      ),
    );
  }

  for (const test of tests) {
    // flowErrors contains the current flow errors. If a step doesn't care
    // about flow errors, we won't check for the current flow errors and
    // flowErrors will contain null
    let flowErrors = noErrors;

    let testBuilder = await builder.createFreshTest(
      bin,
      suiteName,
      testNum,
      test.flowConfigFilename,
    );
    let stepResults = [];
    const steps = [].concat(testSuite.getBeforeEach(emptyTestStep), test.steps);

    for (const step of steps) {
      if (!(step instanceof TestStep)) {
        throw new Error(format("Expected a TestStep, instead got", step));
      }
      printStatus('RUN');

      if (step.needsFlowServer()) {
        // No-op if one is already running
        await testBuilder.startFlowServer();
      }

      // If one of the assertions will check flow errors, then we need to make
      // sure we have the current errors BEFORE doing any actions
      if (flowErrors == null && step.needsFlowCheck()) {
        flowErrors = await testBuilder.getFlowErrors();
      }
      let { envRead, envWrite } = newEnv(flowErrors || noErrors);

      await step.performActions(testBuilder, envWrite);

      let oldErrors = flowErrors;

      // If any of the actions affect flow and if the assertions check flow
      // errors, then get the new flow errors
      if (envRead.shouldRunFlow() && step.needsFlowCheck()) {
        flowErrors = await testBuilder.getFlowErrors();
        envWrite.setNewErrors(flowErrors);
      } else {
        flowErrors = null;
      }

      let result = step.checkAssertions(envRead);
      if (result.passed) {
        stepsPassed++;
      } else {
        stepsFailed++;
      }
      stepResults.push(result);
    }

    // No-op if nothing is running
    await testBuilder.stopFlowServer();

    testsRun++;
    results.push({
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

  return results;
}

