/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @noformat
 */

import type {FlowResult} from '../flowResult';
import type {AllInvocations} from './ShellMocker';
import type {LSPMessage} from './lsp';

export interface StepEnvWriteable {
  reportStdout(output: string): void;
  reportStderr(output: string): void;
  reportExitCode(code: number): void;
  setLSPMessagesSinceStartOfStep(messages: Array<LSPMessage>): void;
  setLSPStderrSinceStartOfStep(stderr: string): void;
  setMockInvocationsSinceStartOfStep(invocations: AllInvocations): void;
  setNewErrors(errors: FlowResult): void;
  setServerRunning(running: 'stopped' | 'running'): void;
  setLSPRunning(running: 'stopped' | 'running'): void;
  triggerFlowCheck(): void;
}

export interface StepEnvReadable {
  getStdout(): string;
  getStderr(): string;
  getExitCodes(): Array<number>;
  getLSPMessagesSinceStartOfStep(): Array<LSPMessage>;
  getLSPStderrSinceStartOfStep(): string;
  getMockInvocationsSinceStartOfStep(): AllInvocations;
  getOldErrors(): FlowResult;
  getNewErrors(): FlowResult;
  getServerRunning(): 'stopped' | 'running';
  getLSPRunning(): 'stopped' | 'running';
  shouldRunFlow(): boolean;
}

function newEnv(
  oldErrors: FlowResult,
): {envWrite: StepEnvWriteable, envRead: StepEnvReadable} {
  let stdout = [];
  let stderr = [];
  let exitCodes = [];
  let newErrors = oldErrors;
  let serverRunning = 'stopped';
  let lspRunning = 'stopped';
  let shouldRunFlow = false;
  let lspMessagesSinceStartOfStep = [];
  let lspStderrSinceStartOfStep = '';
  let mockInvocationsSinceStartOfStep = {};

  const envWrite: StepEnvWriteable = {
    reportStdout(output) {
      stdout.push(output);
    },

    reportStderr(output) {
      stderr.push(output);
    },

    reportExitCode(code) {
      exitCodes.push(code);
    },

    setLSPMessagesSinceStartOfStep(messages) {
      lspMessagesSinceStartOfStep = messages;
    },

    setLSPStderrSinceStartOfStep(stderr) {
      lspStderrSinceStartOfStep = stderr;
    },

    setMockInvocationsSinceStartOfStep(invocations) {
      mockInvocationsSinceStartOfStep = invocations;
    },

    setNewErrors(errors) {
      newErrors = errors;
    },

    setServerRunning(running) {
      serverRunning = running;
    },

    setLSPRunning(running) {
      lspRunning = running;
    },

    triggerFlowCheck() {
      shouldRunFlow = true;
    },
  };

  const envRead: StepEnvReadable = {
    getStdout() {
      return stdout.join('\n');
    },

    getStderr() {
      return stderr.join('\n');
    },

    getExitCodes() {
      return exitCodes.slice();
    },

    getLSPMessagesSinceStartOfStep() {
      return lspMessagesSinceStartOfStep;
    },

    getLSPStderrSinceStartOfStep() {
      return lspStderrSinceStartOfStep;
    },

    getMockInvocationsSinceStartOfStep() {
      return mockInvocationsSinceStartOfStep;
    },

    getOldErrors() {
      return oldErrors;
    },

    getNewErrors() {
      return newErrors;
    },

    getServerRunning() {
      return serverRunning;
    },

    getLSPRunning() {
      return lspRunning;
    },

    shouldRunFlow() {
      return shouldRunFlow;
    },
  };

  return {envWrite, envRead};
}

module.exports = {
  newEnv,
};
