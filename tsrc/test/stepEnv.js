/* @flow */

import type {FlowResult} from '../flowResult';

export interface StepEnvWriteable {
  reportStdout(output: string): void;
  reportStderr(output: string): void;
  reportExitCode(code: number): void;
  setNewErrors(errors: FlowResult): void;
  setServerPid(pid: ?number): void;
  triggerFlowCheck(): void;
}

export interface StepEnvReadable {
  getStdout(): string;
  getStderr(): string;
  getExitCodes(): Array<number>;
  getOldErrors(): FlowResult;
  getNewErrors(): FlowResult;
  getServerPid(): ?number;
  shouldRunFlow(): boolean;
}

export function newEnv(
  oldErrors: FlowResult,
): { envWrite: StepEnvWriteable, envRead: StepEnvReadable } {
  let stdout = [];
  let stderr = [];
  let exitCodes = [];
  let newErrors = oldErrors;
  let serverPid = undefined;
  let shouldRunFlow = false;

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

    setNewErrors(errors) {
      newErrors = errors;
    },

    setServerPid(pid) {
      serverPid = pid;
    },

    triggerFlowCheck() {
      shouldRunFlow = true;
    }
  }

  const envRead: StepEnvReadable = {
    getStdout() {
      return stdout.join("\n");
    },

    getStderr() {
      return stderr.join("\n");
    },

    getExitCodes() {
      return exitCodes.slice();
    },

    getOldErrors() {
      return oldErrors;
    },

    getNewErrors() {
      return newErrors;
    },

    getServerPid() {
      return serverPid;
    },

    shouldRunFlow() {
      return shouldRunFlow;
    },
  }

  return { envWrite, envRead };
}
