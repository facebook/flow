/* @flow */

import type {FlowResult} from '../flowResult';
import type {IDEMessage} from './ide';

export interface StepEnvWriteable {
  reportStdout(output: string): void;
  reportStderr(output: string): void;
  reportExitCode(code: number): void;
  setIDEMessages(messages: Array<IDEMessage>): void;
  setIDEStderr(stderr: string): void;
  setNewErrors(errors: FlowResult): void;
  setServerPid(pid: ?number): void;
  triggerFlowCheck(): void;
}

export interface StepEnvReadable {
  getStdout(): string;
  getStderr(): string;
  getExitCodes(): Array<number>;
  getIDEMessages(): Array<IDEMessage>;
  getIDEStderr(): string;
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
  let ideMessages = [];
  let ideStderr = "";

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

    setIDEMessages(messages) {
      ideMessages = messages;
    },

    setIDEStderr(stderr) {
      ideStderr = stderr;
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

    getIDEMessages() {
      return ideMessages;
    },

    getIDEStderr() {
      return ideStderr;
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
