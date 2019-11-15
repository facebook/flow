/**
 * @flow
 * @format
 */

import type {FlowResult} from '../flowResult';
import type {LSPMessage} from './lsp';

export interface StepEnvWriteable {
  reportStdout(output: string): void;
  reportStderr(output: string): void;
  reportExitCode(code: number): void;
  setLSPMessagesSinceStartOfStep(messages: Array<LSPMessage>): void;
  setLSPStderrSinceStartOfStep(stderr: string): void;
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
  getOldErrors(): FlowResult;
  getNewErrors(): FlowResult;
  getServerRunning(): 'stopped' | 'running';
  getLSPRunning(): 'stopped' | 'running';
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
  let lspRunning = 'stopped';
  let shouldRunFlow = false;
  let lspMessagesSinceStartOfStep = [];
  let lspStderrSinceStartOfStep = '';

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

    setLSPMessagesSinceStartOfStep(messages) {
      lspMessagesSinceStartOfStep = messages;
    },

    setLSPStderrSinceStartOfStep(stderr) {
      lspStderrSinceStartOfStep = stderr;
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
