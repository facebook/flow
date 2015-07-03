#!/usr/bin/env node
var tests = require("./hardcoded_tests.js");
var runTest = require(".//hardcoded_test_runner.js");
var colors = require("colors");
var parseArgs = require("minimist");

var failures = {};
var num_successes = 0;
var num_failures = 0;

var argv = parseArgs(
  process.argv.slice(2),
  {string: ["filter"]}
);

var todo = {};

function escape_content(content) {
  return content
    .replace(/[\\]/g, '\\\\')
    .replace(/[\/]/g, '\\/')
    .replace(/[\b]/g, '\\b')
    .replace(/[\f]/g, '\\f')
    .replace(/[\n]/g, '\\n')
    .replace(/[\r]/g, '\\r')
    .replace(/[\t]/g, '\\t');
}

function test_section(section) {
  console.log("===%s===".bold, section);
  for (var content in tests[section]) {
    test = {
      content: content,
      spec: tests[section][content]
    };
    test.dumpAst = argv.dumpAst;
    test.jsonErrors = argv.jsonErrors;
    test.showDifferences = argv.showDifferences;
    var name = escape_content(test.content);
    process.stdout.write("RUNNING".yellow + " " + name + "\r");
    var result = runTest(test);
    if (result.passed) {
      console.log('%s: "%s"', 'PASSED'.green, name);
      num_successes++;
    } else {
      console.log('%s: "%s"', 'FAILED'.redBG.white, name);
      num_failures++;

      failures[section] = failures[section] || {};
      failures[section][test.content] = result;
    }
  }
}

function go() {
  if (typeof argv.filter == "string") {
    var regex = new RegExp(argv.filter);
    for (section in tests) {
      if (tests.hasOwnProperty(section)) {
        var foundOne = false;
        for (test in tests[section]) {
          if (test.match(regex)) {
            foundOne = true;
          } else {
            delete tests[section][test];
          }
        }
        if (!foundOne) {
          delete tests[section];
        }
      }
    }
  } else if (typeof argv.filter != "undefined") {
    console.log("Filter must be a string, given %s", typeof argv.filter);
    return usage();
  }
  for (prop in tests) {
    if (todo[prop]) {
      delete tests[prop];
    }
  }
  if (argv.dumpAst) {
    var num_tests = 0;
    for (prop in tests) {
      if (tests.hasOwnProperty(prop)) {
        num_tests += tests[prop].length;
      }
    }
    if (num_tests > 20) {
      console.log(
        "Oh summer child, you really don't want to dump the Ast for %d tests. " +
        "Try using --filter to run fewer tests",
        num_tests
      );
      return usage();
    }
  }

  for (prop in tests) {
    if (tests.hasOwnProperty(prop)) {
      test_section(prop);
    }
  }

  console.log("%d/%d tests passed", num_successes, num_successes + num_failures);

  if (num_failures > 0) {
    console.log("*** %d TESTS FAILED! ***".redBG.white, num_failures);

    for (section in failures) {
      if (failures.hasOwnProperty(section)) {
        console.log("===%s Failures===".bold, section);
        for (test in failures[section]) {
          if (failures[section].hasOwnProperty(test)) {
            var result = failures[section][test];
            console.log('Test failure: "%s"'.redBG.white, escape_content(test));
            console.log(result.output);
          }
        }
      }
    }
    process.exit(1);
  }
}

function usage() {
  console.log("usage: %s [OPTIONS]", process.argv[0]);
  console.log("Supported options");
  console.log("\t--dumpAst", "Dumps the esprima & flow ASTs before each test");
  console.log("\t--filter=regex", "Only run tests that match the regex");
  console.log("\t--jsonErrors", "Output errors in json format");
}

if (argv.help) {
  usage();
} else {
  go();
}
