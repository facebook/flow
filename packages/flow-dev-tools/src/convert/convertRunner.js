/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import colors from 'colors/safe';
import {basename, dirname, extname, join, normalize, relative} from 'path';
import {format} from 'util';

import {
  exists,
  glob,
  mkdirp,
  ncp,
  readdir,
  readFile,
  rename,
  rimraf,
  unlink,
  writeFile,
} from '../utils/async';
import runRecord from '../record/recordRunner';
import {getTestsDir, defaultFlowConfigName} from '../constants';

import simpleDiffAssertion from '../test/assertions/simpleDiffAssertion';

import type {Args} from './convertCommand';

async function shouldConvert(source, log? = (...args) => {}) {
  const source_exists = await exists(source);
  if (!source_exists) {
    log("'%s' doesn't exist", source);
    return false;
  }

  const contents = await readdir(source);
  if (contents.find(filename => filename == ".testconfig")) {
    log("Sorry, can't convert legacy tests that use .testconfig");
    return false;
  }

  if (!contents.find(filename => extname(filename) == ".exp")) {
    log("Legacy test is missing .exp file");
    return false;
  }

  function isInteresting(file) {
    switch (file) {
      case '.testconfig':
      case basename(source)+".exp":
        return false;
      default:
        return true;
    }
  }

  const interesting_file_count =
    contents
      .filter(isInteresting)
      .length;

  if (interesting_file_count == 0) {
    log("Legacy test doesn't have any interesting files");
    return false;
  }
  return true;
}

function logWithSource(source: string, ...args: any) {
  const padding = Array(Math.max(25 - source.length, 2)).join(" ");
  console.log("[%s]%s%s", source, padding, format(...args));
}

async function convert(
  no_flowlib: "true" | "false",
  results: {[key: string]: ?string},
  source: string,
): Promise<?string> {
  results[source] = null;
  const log = logWithSource.bind(null, source);

  const suiteName = join("legacy", basename(source));
  const dest = join(getTestsDir(), suiteName);

  log("Converting test to `%s`", dest);
  const goodToGo = await shouldConvert(source, log);
  if (!goodToGo) {
    return;
  }

  log("Removing directory `%s`", dest);
  await rimraf(dest);


  log("Creating directory `%s`", dest);
  await mkdirp(dest);

  try {
    await ncp(source, dest, {
      filter: filename => !filename.match(/.*\.exp$/),
      dereference: true,
      stopOnErr: true,
    });
  } catch (e) {
    log("ncp failed: %s", e);
    return;
  }

  const flow_files = await glob(
    format("%s/**/*", dest),
    { cwd: __dirname, nodir: true, dot: true },
  );

  const converted_files = [];
  for (const file of flow_files) {
    const fileContents = await readFile(file);
    await writeFile(
      file,
      fileContents
        .replace(/@flow/, "@thisWillBeFlowInTest")
        .replace(/[\/\\]test(['"])/g, "/test\.legacy$1")
        .replace(/[\/\\]test\.js/, "/test\.legacy.js"),
    );

    if (basename(file) == "test.js") {
      const new_file_name = join(dirname(file), "test.legacy.js");
      converted_files.push(relative(dest, new_file_name));
      await rename(file, new_file_name);
    } else if (basename(file) == ".flowconfig") {
      const config = await readFile(file);
      let newConfig = config.toString();
      // Old tests ran flow check --all, so automatically add that option
      if (newConfig.match(/\[options\]/)) {
        newConfig = newConfig.replace(
          /\[options\]/,
          format("[options]\nall=true\nno_flowlib=%s\n", no_flowlib),
        );
      } else {
        newConfig = format(
          "%s\n[options]\nall=true\nno_flowlib=%s\n",
          newConfig,
          no_flowlib,
        );
      }
      // Rename .flowconfig to _flowconfig. This will be automatically included
      // in the test.
      await Promise.all([
        writeFile(join(dirname(file), defaultFlowConfigName), newConfig),
        unlink(file),
      ]);
    } else {
      converted_files.push(relative(dest, file));
    }
  }

  let file_list = converted_files
    .sort()
    .map(file => format("'%s',", file.replace(/\\/g, '/')))
    .join("\n      ");

  await writeFile(join(dest, "test.js"),
`/* Copyright Facebook
 * @flow
 */
import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({addFiles}) => [
  test('legacy test', [
    addFiles(
      ${file_list}
    ).noNewErrors(),
  ]),
]);
`,
  );

  results[source] = suiteName;
  return suiteName;
}

async function sanityCheck(sourceToSuiteMap) {
  console.log("Running sanity checks...");
  let passed = 0;
  let failed = new Set();
  let skipped = 0;
  for (const source in sourceToSuiteMap) {
    const suiteName = sourceToSuiteMap[source];
    if (suiteName == null ) {
      console.log(
        "%s: %s",
        colors.yellow.bold("[-] SKIP"),
        colors.blue(source),
      );
      skipped++;
      continue;
    }
    const expfile =
      join(source, suiteName.replace(/legacy[\/\\](.*)/, "$1.exp"));
    const [exp, testFileContentsBuffer] = await Promise.all([
      readFile(expfile),
      readFile(join(getTestsDir(), suiteName, "test.js")),
    ]);
    const testFileContents = testFileContentsBuffer.toString();
    let matches = exp.toString().match(/Found (.*) errors?/);
    if (!matches) {
      console.log(
        "%s: %s %s",
        colors.red.bold("[\u{2717}] FAIL"),
        colors.blue(suiteName),
        format("Couldn't find `Found _ errors` in %s", expfile),
      );
      failed.add(suiteName);
      continue;
    }
    const numErrors = Number(matches[1]);
    if (numErrors === 0) {
      if (testFileContents.match(/noNewErrors\(\)/) == null) {
        console.log(
          "%s: %s %s",
          colors.red.bold("[\u{2717}] FAIL"),
          colors.blue(suiteName),
          format(
            "%s says 0 errors, but %s doesn't use noNewErrors()",
            expfile,
            suiteName,
          ),
        );
        failed.add(suiteName);
        continue;
      }
    } else {
      matches = testFileContents.match(/newErrors\(\n *`((.|[\n])*)`,\n *\)/);
      if (matches == null) {
        console.log(
          "%s: %s %s",
          colors.red.bold("[\u{2717}] FAIL"),
          colors.blue(suiteName),
          format(
            "%s says %d errors, but %s doesn't use newErrors()",
            expfile,
            numErrors,
            suiteName,
          ),
        );
        failed.add(suiteName);
        continue;
      } else {
        // There are a few things we need to do to make the old test output match
        // * Remove the "Found N errors" message
        // * Rename <BUILTINS>/ to [LIB] (which is what the JSON outputs)
        // * Escape things for template strings
        // * Do the test -> test.legacy rename
        const actual = matches[1]
          .replace(/test\.legacy/g, "test")
          .split("\n")
          .map(s => s.trim());
        const expected = exp.toString()
          .replace(/Found .* errors?/, "")
          .replace(/<BUILTINS>\//g, "[LIB] ")
          .replace(/See lib: ([^[])/g, 'See lib: [LIB] $1')
          .replace(/`/g, '\\`')
          .replace(/\$/g, '\\$')
          .split("\n")
          .map(s => s.trim());
        let doesNotMatch = false;

        for (let eidx = 0, aidx = 0; eidx < expected.length && aidx < actual.length;) {
          if (expected[eidx] === "") {
            eidx++;
            continue;
          }
          if (actual[aidx] === "") {
            aidx++;
            continue;
          }
          if (actual[aidx] != expected[eidx]) {
            console.log(
              "%s: %s %s",
              colors.red.bold("[\u{2717}] FAIL"),
              colors.blue(suiteName),
              format(
                "Line %d of %s doesn't seem to match the recorded test.js\n"+
                  "Expected `%s`\nActual `%s`",
                eidx+1,
                expfile,
                expected[eidx],
                actual[aidx],
              ),
            );
            failed.add(suiteName);
            doesNotMatch = true;
            break;
          }
          eidx++;
          aidx++;
        }
        if (doesNotMatch === true) {
          continue;
        }
      }
    }
    console.log("%s: %s", colors.green.bold("[\u{2713}] PASS"), colors.blue(suiteName));
    passed++;
  }
  console.log(
    colors.green("%d passed") + ", " + colors.red("%d failed") + ", " + colors.yellow("%d skipped"),
    passed,
    failed.size,
    skipped
  );

  return failed;
}

async function convertAll(
  args: Args,
  no_flowlib: "true" | "false",
  dirs: Array<string>,
  sourceToSuiteMap: {[key: string]: ?string},
): Promise<void> {
  console.log("Converting with no_flowlib=%s", no_flowlib);
  const converted: Array<?string> = await Promise.all(
    dirs.map(convert.bind(null, no_flowlib, sourceToSuiteMap))
  );
  const suites = new Set();
  converted.forEach(suiteName => suiteName != null && suites.add(suiteName));

  await runRecord({
    suites,
    bin: args.bin,
    parallelism: args.parallelism,
    errorCheckCommand: args.errorCheckCommand,
    rerun: null,
  });
}


export default async function(args: Args): Promise<void> {
  const sourceToSuiteMap = {};
  const suiteToSourceMap = {};

  const dirs = Array.from(args.dirs).map(dir => normalize(dir.trim()));

  if (!args.sanity) {
    await convertAll(args, "true", dirs, sourceToSuiteMap);
  } else {
    // Fake like we generate the tests
    console.log("Skipping straight to the sanity check due to `--sanity` flag");
    for (const source of dirs) {
      const goodToGo = await shouldConvert(source);
      sourceToSuiteMap[source] = goodToGo ? join("legacy", basename(source)) : null;
    }
  }

  for (const dir in sourceToSuiteMap) {
    suiteToSourceMap[sourceToSuiteMap[dir]] = dir;
  }

  let failed = await sanityCheck(sourceToSuiteMap);

  if (!args.sanity && failed.size > 0) {
    console.log("Trying again...");

    const failed_dirs = Array.from(failed)
      .map(suiteName => suiteToSourceMap[suiteName]);

    await convertAll(args, "false", failed_dirs, sourceToSuiteMap);
    failed = await sanityCheck(sourceToSuiteMap);
  }

  if (!args.sanity) {
    await Promise.all(
      dirs.map(async (dir) => {
        const suiteName = sourceToSuiteMap[dir];
        if (suiteName == null) {
          logWithSource(dir, "Convert skipped");
        } else {
          if (failed.has(suiteName)) {
            const dest = join(getTestsDir(), suiteName);
            logWithSource(dir, "Convert failed, removing dest `%s`", dest);
            return rimraf(dest);
          } else {
            logWithSource(dir, "Convert succeeded, removing source `%s`", dir);
            await rimraf(dir);
          }
        }
      }),
    );
  }
}
