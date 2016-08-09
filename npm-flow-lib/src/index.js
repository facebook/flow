// @flow

import type {TProgram} from "./astTypes";
import {cmd} from "./lib/comm";

/**
 * `flow ast < fileContents`
 */
export async function astFromFileContents(
  cwd: string,
  fileContents: string
): Promise<TProgram> {
  const [res, err] = await cmd(cwd, "ast", [], [], fileContents);
  if (res == null) {
    throw new Error(err);
  }
  return res;
}

/**
 * `flow ast file.js`
 */
export async function astFromFilePath(
  cwd: string,
  filePath: string
): Promise<TProgram> {
  const [res, err] = await cmd(cwd, "ast", [], [filePath]);
  if (res == null) {
    throw new Error(err);
  }
  return res;
}

/**
 * `flow check-contents < fileContents`
 */
type CheckContentsResult = {
  passed: boolean,
  errors: Array<{
    message: Array<{
      descr: string,
      level: string,
      path: string,
      line: number,
      endline: number,
      start: number,
      end: number,
    }>,
    kind: string,
  }>,
  version: string,
};
export async function checkContents(
  cwd: string,
  fileContents: string
): Promise<CheckContentsResult> {
  const [res, err] = await cmd(cwd, "check-contents", [], [], fileContents);
  if (res == null) {
    throw new Error(err);
  }
  return res;
}
