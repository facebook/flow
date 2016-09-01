/* @flow */

import colors from 'colors/safe';
import {basename, dirname, extname, join, normalize, relative} from 'path';
import {format} from 'util';

import {glob, mkdirp, ncp, readdir, readFile, rename, rimraf, unlink, writeFile} from './../async';
import runRecord from '../record/recordRunner';
import {getTestsDir, defaultFlowConfigName} from '../constants';

import type {Args} from './convertCommand';

async function shouldConvert(source, log? = () => {}) {
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

async function convert(
  results: {[key: string]: ?string},
  source: string,
): Promise<?string> {
  results[source] = null;
  function log(...args: any) {
    console.log("[%s]\t\t%s", source, format(...args));
  }

  const suiteName = join("legacy", basename(source));
  const dest = join(getTestsDir(), suiteName);

  log("Removing directory `%s`", dest);
  await rimraf(dest);
  log("Converting test to `%s`", dest);

  const goodToGo = await shouldConvert(source, log);
  if (!goodToGo) {
    return;
  }

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
        .replace(/[\/\\]test(['"])/g, "/legacy_test$1")
        .replace(/[\/\\]test\.js/, "/legacy_test.js"),
    );

    if (basename(file) == "test.js") {
      const new_file_name = join(dirname(file), "legacy_test.js");
      converted_files.push(relative(dest, new_file_name));
      await rename(file, new_file_name);
    } else if (basename(file) == ".flowconfig") {
      const config = await readFile(file);
      let newConfig = config.toString();
      // Old tests ran flow check --all, so automatically add that option
      if (newConfig.match(/\[options\]/)) {
        newConfig = newConfig.replace(/\[options\]/, "[options]\nall=true\n");
      } else {
        newConfig = newConfig + "\n[options]\nall=true\n";
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
`/* @flow */
import {suite, test} from '../../../tsrc/test/Tester';

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
  let failed = 0;
  let skipped = 0;
  for (const source in sourceToSuiteMap) {
    const suiteName = sourceToSuiteMap[source];
    if (suiteName == null ) {
      console.log("%s: %s", colors.yellow.bold("[-] SKIP"), colors.blue(source));
      skipped++;
      continue;
    }
    const expfile = join(source, suiteName.replace(/legacy[\/\\](.*)/, "$1.exp"));
    const [exp, testFileContentsBuffer] = await Promise.all([
      readFile(expfile),
      readFile(join(getTestsDir(), suiteName, "test.js")),
    ]);
    const testFileContents = testFileContentsBuffer.toString();
    let matches = exp.toString().match(/Found (.*) errors?/);
    if (!matches) {
      console.log(
        "%s: %s %s",
        colors.red.bold("[✗] FAIL"),
        colors.blue(suiteName),
        format("Couldn't find `Found _ errors` in %s", expfile),
      );
      failed++;
      continue;
    }
    const numErrors = Number(matches[1]);
    if (testFileContents.match(/noNewErrors\(\)/) && numErrors != 0) {
      console.log(
        "%s: %s %s",
        colors.red.bold("[✗] FAIL"),
        colors.blue(suiteName),
        format("%s says %d errors, but %s uses noNewErrors()", expfile, numErrors, suiteName),
      );
      failed++;
      continue;
    } else if (matches = testFileContents.match(/newErrors\(\n`((.|[\n])*)`,\n\)/)) {
      const numNewErrors =
        matches[1]
          .split("\n\n")
          .length;
      if (numNewErrors != numErrors) {
        console.log(
          "%s: %s %s",
          colors.red.bold("[✗] FAIL"),
          colors.blue(suiteName),
          format("%s says %d errors, but %s has %d errors", expfile, numErrors, suiteName, numNewErrors),
        );
        failed++;
        continue
      }
    }
    console.log("%s: %s", colors.green.bold("[✓] PASS"), colors.blue(suiteName));
    passed++;
  }
  console.log(
    colors.green("%d passed") + ", " + colors.red("%d failed") + ", " + colors.yellow("%d skipped"),
    passed,
    failed,
    skipped
  );
}

export default async function(args: Args): Promise<void> {
  const sourceToSuiteMap = {};

  const dirs = Array.from(args.dirs).map(dir => normalize(dir.trim()));

  if (!args.sanity) {
    const converted: Array<?string> = await Promise.all(
      dirs.map(convert.bind(null, sourceToSuiteMap))
    );
    const suites = new Set(converted.filter(suiteName => suiteName != null));

    await runRecord({
      suites,
      bin: args.bin,
      parallelism: args.parallelism,
      errorCheckCommand: args.errorCheckCommand,
      rerun: null,
    });
  } else {
    // Fake like we generate the tests
    console.log("Skipping straight to the sanity check due to `--sanity` flag");
    for (const source of dirs) {
      const goodToGo = await shouldConvert(source);
      sourceToSuiteMap[source] = goodToGo ? join("legacy", basename(source)) : null;
    }
  }

  await sanityCheck(sourceToSuiteMap);
}
