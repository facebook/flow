#!/usr/bin/env node
/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

var fs = require('fs');
var path = require('path');
var colors = require("colors");
var parseArgs = require("minimist");

var argv = parseArgs(
  process.argv.slice(2),
  {
    string: [
      "esprima-tests",
      "hardcoded-tests",
      "filter",
      "section-filter",
      "numTests",
      "parser",
    ],
    boolean: [
      "dumpAst",
      "help",
      "jsonErrors",
      "showDifferences",
    ],
    unknown: function(flag) {
      if (flag.match(/^-/)) {
        console.log('Unsupported flag:', flag, "\n");
        usage();
      }
    }
  }
);

var cli_options = {
  dumpAst: argv.dumpAst,
  jsonErrors: argv.jsonErrors,
  showDifferences: argv.showDifferences,
  flow: require(argv.parser || "../flow_parser.js"),
};

function escape_content(content) {
  return content
    .replace(/[\\]/g, '\\\\')
    .replace(/[\b]/g, '\\b')
    .replace(/[\f]/g, '\\f')
    .replace(/[\n]/g, '\\n')
    .replace(/[\r]/g, '\\r')
    .replace(/[\t]/g, '\\t');
}

function apply_filters(sections) {
  var numTests = Number(argv.numTests);
  var section_filter = new RegExp(argv['section-filter'] || '');
  var test_filter = new RegExp(argv.filter || '');
  var filtered_sections = {};
  for (var section in sections) {
    if (sections.hasOwnProperty(section) && section_filter.test(section)) {
      var tests_to_filter = Array.isArray(sections[section]) ?
        sections[section] :
        sections[section].tests;
      var filtered_tests = tests_to_filter.filter(function(test) {
        return test_filter.test(test.content);
      });
      if (filtered_tests.length > 0) {
        if (numTests !== undefined) {
          numTests -= filtered_tests.length;
          if (numTests < 0) {
            filtered_tests = filtered_tests.slice(0, numTests);
          }
        }
        filtered_sections[section] = sections[section];
        filtered_sections[section].tests = filtered_tests;
      }
    }
    if (numTests !== undefined && numTests < 0) {
      break;
    }
  }
  return filtered_sections;
}

function get_esprima_tests() {
  var spec = require(
    argv['esprima-tests'] ?
      path.resolve(argv['esprima-tests']) :
      '../../../src/parser/test/esprima_tests.js'
  );

  var sections = {};
  for (var section_name in spec.sections) {
    // Remove TODO sections
    if (!spec.sections.hasOwnProperty(section_name) ||
        spec.todo[section_name] === true) {
      continue;
    }

    var section = spec.sections[section_name];
    if (Array.isArray(section)) {
      section = {
        options: {},
        tests: section,
      };
    }

    // Normalize the tests
    section.tests = section.tests.map(function(test) {
      return typeof test === "string" ? { content: test } : test;
    });
    sections[section_name] = section;
  }

  return sections;
}

function list_files(root, dir) {
  var files = fs.readdirSync(dir ? path.join(root, dir) : root);
  var result = [];
  for (var i = 0; i < files.length; i++) {
    var file = dir ? path.join(dir, files[i]) : files[i];
    var stats = fs.statSync(path.join(root, file));
    if (stats.isDirectory()) {
      result = result.concat(list_files(root, file));
    } else {
      result.push(file);
    }
  }
  return result.sort();
}

function get_tests(root_dir) {
  var files = list_files(root_dir);
  var tests = {};
  for (var i = 0; i < files.length; i++) {
    var file = files[i];
    var test_name = path.dirname(file);
    var case_parts = path.basename(file).split('.');
    var case_name = case_parts[0];

    // Hack to ignore hidden files.
    if (case_name === '') {
      continue;
    }

    var cases = (tests[test_name] = tests[test_name] || {});
    var case_ = (cases[case_name] = cases[case_name] || {});

    var content = fs.readFileSync(
      path.join(root_dir, file),
      { encoding: 'utf8' }
    );
    var ext = case_parts[case_parts.length - 1];
    var kind = case_parts.length > 2 ? case_parts[case_parts.length - 2] : null;

    if (ext === "js") {
      case_.content = content;
    } else if (ext === "json" && kind === "tree") {
      case_.expected_ast = JSON.parse(content);
    } else if (ext === "json" && kind === "options") {
      case_.options = JSON.parse(content);
    }
  }
  return tests;
}

function get_hardcoded_tests() {
  var tests = get_tests(
    argv['hardcoded-tests'] ?
      path.resolve(argv['hardcoded-tests']) :
      path.resolve(__dirname, '../../../src/parser/test/flow')
  );
  var result = {};
  for (var section in tests) {
    if (tests.hasOwnProperty(section)) {
      var test = tests[section];
      var cases = [];
      // TODO: use Object.values if we require new enough node
      for (var case_ in test) {
        if (test.hasOwnProperty(case_)) {
          cases.push(test[case_]);
        }
      }
      result[section] = { tests: cases };
    }
  }
  return result;
}

function test_section(runTest, section_name, section) {
  console.log("===%s===".bold, section_name);
  var esprima_opts = {};
  var results = {
    num_successes: 0,
    num_failures: 0,
    failures: {},
  };
  for (var i = 0; i < section.tests.length; i++) {
    var test = section.tests[i];
    var name = escape_content(test.content);
    process.stdout.write("RUNNING".yellow + " " + name + "\r");

    var options = {};
    for (var key in section.options) {
      if (section.options.hasOwnProperty(key)) {
        options[key] = section.options[key];
      }
    }
    for (var key in test.options) {
      if (test.options.hasOwnProperty(key)) {
        options[key] = test.options[key];
      }
    }

    var result = runTest(test, options, cli_options);
    if (result.passed) {
      console.log('%s: "%s"', 'PASSED'.green, name);
      results.num_successes++;
    } else {
      console.log('%s: "%s"', 'FAILED'.redBG.white, name);
      results.num_failures++;

      results.failures[test.content] = result;
    }
  }
  return results;
}

function report_percentage_passed(results) {
  var passed = results.num_failures === 0;
  var result_fraction = passed ?
    "%s/%s".green :
    "%s/%s".redBG.white;
  console.log(
    "%s test suite: ".bold + result_fraction + " tests passed",
    results.suite,
    results.num_successes,
    results.num_successes + results.num_failures
  );
  return passed;
}

function report_results(results) {
  var passed = report_percentage_passed(results);

  if (!passed) {
    console.log(
      "*** %d TESTS FAILED! ***".redBG.white,
      results.num_failures
    );

    for (var section_name in results.failures) {
      if (results.failures.hasOwnProperty(section_name)) {
        console.log("===%s Failures===".bold, section_name);
        for (var test in results.failures[section_name]) {
          if (results.failures[section_name].hasOwnProperty(test)) {
            var result = results.failures[section_name][test];
            console.log('Test failure: "%s"'.redBG.white, escape_content(test));
            console.log(result.output);
          }
        }
      }
    }
  }
  return passed;
}

function run_test_suite(test) {
  console.log(
    "\n\n*****RUNNING %s TEST SUITE*****".bold,
    test.suite.toUpperCase()
  );
  var exit_code = 0;

  var sections = apply_filters(test.sections);
  var results = {
    suite: test.suite,
    num_successes: 0,
    num_failures: 0,
    failures: {},
  };
  for (var section_name in sections) {
    if (sections.hasOwnProperty(section_name)) {
      var section_results =
        test_section(test.runner, section_name, sections[section_name]);
      results.num_successes += section_results.num_successes;
      results.num_failures += section_results.num_failures;
      if (section_results.num_failures > 0) {
        results.failures[section_name] = section_results.failures;
      }
    }
  }
  report_results(results);

  return results;
}

function go() {
  var suites = {
    esprima: false,
    hardcoded: false,
  };
  if (argv._.length === 0) {
    argv._.push('all');
  }
  for (var i = 0; i < argv._.length; i++) {
    switch (argv._[i]) {
      case 'esprima':
        suites.esprima = true;
        break;
      case 'hardcoded':
        suites.hardcoded = true;
        break;
      case 'all':
        suites.esprima = true;
        suites.hardcoded = true;
        break;
      default:
        console.log('Unknown suite specifier "%s"', argv._[i]);
        return usage();
    }
  }

  var tests = [];

  if (suites.esprima) {
    tests.push({
      suite: 'esprima-fb compatibility',
      runner: require('./esprima_test_runner.js'),
      sections: get_esprima_tests(),
    });
  }
  if (suites.hardcoded) {
    tests.push({
      suite: 'hardcoded',
      runner: require('./hardcoded_test_runner.js'),
      sections: get_hardcoded_tests(),
    });
  }

  var suite_results = tests.map(run_test_suite);

  console.log(
    "\n%d suite%s run",
    suite_results.length,
    suite_results.length === 1 ? '' : 's'
  );
  var exit_code = 0;
  suite_results.forEach(function (results) {
    report_percentage_passed(results);
    if (results.num_failures !== 0) {
      exit_code = 1;
    }
  });
  process.exit(exit_code);
}

function usage() {
  console.log("usage: %s [OPTIONS] [all] [esprima] [hardcoded]", process.argv[1]);
  console.log("Supported options");
  console.log("\t--esprima-tests", "Path to legacy esprima tests");
  console.log("\t--hardcoded-tests", "Directory containing the hardcoded tests");
  console.log("\t--dumpAst", "Dumps the esprima & flow ASTs before each test");
  console.log("\t--filter=regex", "Only run tests that match the regex");
  console.log("\t--section-filter=regex", "Only run tests from sections that match the regex");
  console.log("\t--jsonErrors", "Output errors in json format");
  console.log("\t--numTests=n", "Run at most n tests");
  console.log(
    "\t--showDifferences",
    "Only run the tests for which esprima and flow differ"
  );
  process.exit(1);
}

if (argv.help) {
  usage();
} else {
  go();
}
