/* @flow */

import {resolve} from 'path';


export const defaultTestsDirName = "newtests";

// This is where we look for tests to run and where we put newly converted
// tests
export function getTestsDir(relative_to?: string) {
  if (relative_to !== undefined) {
    return resolve(relative_to, defaultTestsDirName);
  } else {
    return resolve(__dirname, "../", defaultTestsDirName);
  }
}

export const binOptions: Array<string> = [
  resolve(__dirname, "../bin/flow"), // Open source build
  resolve(__dirname, "../bin/flow.exe"), // Open source windows build
  resolve(__dirname, "../../buck-out/gen/flow/flow/flow"), // Buck
  resolve(__dirname, "../../_bin/flow/flow"), // Fbmake
];

export const defaultFlowConfigName = "_flowconfig";
