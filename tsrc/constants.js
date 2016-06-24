/* @flow */

import {resolve} from 'path';

// This is where we look for tests to run and where we put newly converted
// tests
export const testsDir = resolve(__dirname, "../newtests")

export const binOptions: Array<string> = [
  resolve(__dirname, "../bin/flow"), // Open source build
  resolve(__dirname, "../bin/flow.exe"), // Open source windows build
  resolve(__dirname, "../../buck-out/gen/flow/flow/flow"), // Buck
  resolve(__dirname, "../../_bin/flow/flow"), // Fbmake
];

export const defaultFlowConfigName = "_flowconfig";
