/**
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import type {FlowResult} from '../flowResult';
import type {IDEMessage} from './ide';

export interface StepEnvWriteable {
  reportStdout(output: string): void;
  reportStderr(output: string): void;
  reportExitCode(code: number): void;
  setIDEMessagesSinceStartOfStep(messages: Array<IDEMessage>): void;
  setIDEStderrSinceStartOfStep(stderr: string): void;
  setNewErrors(errors: FlowResult): void;
  setServerRunning(running: 'stopped' | 'running'): void;
  setIDERunning(running: 'stopped' | 'running'): void;
  triggerFlowCheck(): void;
}

export interface StepEnvReadable {
  getStdout(): string;
  getStderr(): string;
  getExitCodes(): Array<number>;
  getIDEMessagesSinceStartOfStep(): Array<IDEMessage>;
  getIDEStderrSinceStartOfStep(): string;
  getOldErrors(): FlowResult;
  getNewErrors(): FlowResult;
  getServerRunning(): 'stopped' | 'running';
  getIDERunning(): 'stopped' | 'running';
  shouldRunFlow(): boolean;
}

export function newEnv(
  oldErrors: FlowResult,
): {envWrite: StepEnvWriteable, envRead: StepEnvReadable} {
  let stdout = [];
  let stderr = [];
  let exitCodes = [];
  let newErrors = oldErrors;
  let serverRunning = 'stopped';
  let ideRunning = 'stopped';
  let shouldRunFlow = false;
  let ideMessagesSinceStartOfStep = [];
  let ideStderrSinceStartOfStep = '';

  const envWrite = {
    reportStdout(output) {
      stdout.push(output);
    },

    reportStderr(output) {
      stderr.push(output);
    },

    reportExitCode(code) {
      exitCodes.push(code);
    },

    setIDEMessagesSinceStartOfStep(messages) {
      ideMessagesSinceStartOfStep = messages;
    },

    setIDEStderrSinceStartOfStep(stderr) {
      ideStderrSinceStartOfStep = stderr;
    },

    setNewErrors(errors) {
      newErrors = errors;
    },

    setServerRunning(running) {
      serverRunning = running;
    },

    setIDERunning(running) {
      ideRunning = running;
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

    getIDEMessagesSinceStartOfStep() {
      return ideMessagesSinceStartOfStep;
    },

    getIDEStderrSinceStartOfStep() {
      return ideStderrSinceStartOfStep;
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

    getIDERunning() {
      return ideRunning;
    },

    shouldRunFlow() {
      return shouldRunFlow;
    },
  };

  return {envWrite, envRead};
}
