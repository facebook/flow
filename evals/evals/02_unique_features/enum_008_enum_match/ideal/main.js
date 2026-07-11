/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum BuildStep {
  Lint,
  Compile,
  Test,
  Package,
  Deploy,
}

export enum ExitCode of number {
  Success = 0,
  Warning = 1,
  Failure = 2,
}

export function stepDuration(step: BuildStep): number {
  return match (step) {
    BuildStep.Lint => 5,
    BuildStep.Compile => 30,
    BuildStep.Test => 60,
    BuildStep.Package => 15,
    BuildStep.Deploy => 45,
  };
}

export function summarizeResult(step: BuildStep, code: ExitCode): string {
  const name: string = step as string;
  return match (code) {
    ExitCode.Success => `${name} passed`,
    ExitCode.Warning => `${name} passed with warnings`,
    ExitCode.Failure => `${name} FAILED`,
  };
}

export function shouldAbort(step: BuildStep, code: ExitCode): boolean {
  return match (code) {
    ExitCode.Failure => match (step) {
      BuildStep.Compile | BuildStep.Deploy => true,
      BuildStep.Lint | BuildStep.Test | BuildStep.Package => false,
    },
    ExitCode.Success | ExitCode.Warning => false,
  };
}
