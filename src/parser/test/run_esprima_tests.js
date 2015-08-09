#!/usr/bin/env node

var runTest = require("./esprima_test_runner.js");
var tests = require("./esprima_tests.js");
var colors = require("colors");
var parseArgs = require("minimist");

var todo = {
  "Whitespace": true,
  'Invalid unicode related syntax': true,
  'Invalid Type Annotations': true,
  'ES6: Numeric Literal': true,
  'Array Comprehension': true,
  'Harmony: Modules': true,
  'Harmony: Iterators': true,
  'Harmony: Invalid Class (strawman)': true,
  'ES6: Destructured Parameters': true,
  'ES7 Proposal: Rest Properties' : true,
  'Harmony Invalid syntax': true,
};

var failures = {};
var num_successes = 0;
var num_failures = 0;

var argv = parseArgs(
  process.argv.slice(2),
  {string: ["filter"]}
);

function escape_content(content) {
  return content
    .replace(/[\\]/g, '\\\\')
    .replace(/[\b]/g, '\\b')
    .replace(/[\f]/g, '\\f')
    .replace(/[\n]/g, '\\n')
    .replace(/[\r]/g, '\\r')
    .replace(/[\t]/g, '\\t');
}

function test_section(section) {
  console.log("===%s===".bold, section);
  var sectionTests = tests[section];
  var esprima_opts = {};
  if (!Array.isArray(sectionTests)) {
    esprima_opts = sectionTests.esprima_opts;
    sectionTests = sectionTests.tests;
  }
  for (var i = 0; i < sectionTests.length; i++) {
    if (num_successes + num_failures >= (argv.numTests || 100000)) {
      break;
    }
    var test = sectionTests[i];
    if (typeof test == "string") {
      test = { content: test };
    }
    test.dumpAst = argv.dumpAst;
    test.jsonErrors = argv.jsonErrors;
    test.showDifferences = argv.showDifferences;
    var name = escape_content(test.content);
    process.stdout.write("RUNNING".yellow + " " + name + "\r");
    var result = runTest(test, esprima_opts);
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

function filter_sections(fn) {
  for (prop in tests) {
    if (tests.hasOwnProperty(prop)) {
      var tests_to_filter =
        Array.isArray(tests[prop]) ? tests[prop] : tests[prop].tests;
      var tests_post_filter = tests_to_filter.filter(fn);
      if (tests_post_filter.length == 0) {
        delete tests[prop];
      } else if (Array.isArray(tests[prop])) {
        tests[prop] = tests_post_filter;
      } else {
        tests[prop].tests = tests_post_filter;
      }
    }
  }
}

function go() {
  if (typeof argv.filter == "string") {
    var regex = new RegExp(argv.filter);
    filter_sections(function (s) {
      if (typeof s == "object") {
        return s.content.match(regex);
      } else {
        return s.match(regex);
      }
    });
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
        num_tests +=
          (Array.isArray(tests[prop]) ? tests[prop] : tests[prop].tests).length;
      }
    }
    if (num_tests > 20 && ((argv.numTests || 100) > 20)) {
      console.log(
        "Oh summer child, you really don't want to dump the Ast for %d tests. " +
        "Try using --filter to run fewer tests",
        num_tests
      );
      return usage();
    }
  }
  if (argv.showDifferences) {
    filter_sections(function (test) {
      return typeof test == "object" && typeof test.expected_differences != "undefined";
    });
  }

  var section_filter = new RegExp(argv.filterSection || '');
  for (section in tests) {
    if (num_successes + num_failures >= (argv.numTests || 100000)) {
      break;
    }
    if (tests.hasOwnProperty(section)) {
      if (section.match(section_filter)) {
        test_section(section);
      }
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
    console.log("%d/%d tests passed", num_successes, num_successes + num_failures);
  }
}

function usage() {
  console.log("usage: %s [OPTIONS]", process.argv[0]);
  console.log("Supported options");
  console.log("\t--dumpAst", "Dumps the esprima & flow ASTs before each test");
  console.log("\t--filter=regex", "Only run tests that match the regex");
  console.log("\t--filterSection=regex", "Only run tests from sections that match the regex");
  console.log("\t--jsonErrors", "Output errors in json format");
  console.log("\t--numTests=n", "Run at most n tests");
  console.log(
    "\t--showDifferences",
    "Only run the tests for which esprima and flow differ"
  );
}

if (argv.help) {
  usage();
} else {
  go();
}
