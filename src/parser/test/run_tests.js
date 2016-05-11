#!/usr/bin/env node

var colors = require("colors");
var parseArgs = require("minimist");

var argv = parseArgs(
  process.argv.slice(2),
  {
    string: [
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
  var spec = require('./esprima_tests.js');

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

function get_hardcoded_tests() {
  var spec = require('./hardcoded_tests.js');

  var sections = {};
  for (var section_name in spec.sections) {
    // Remove TODO sections
    if (!spec.sections.hasOwnProperty(section_name) ||
        spec.todo[section_name] === true) {
      continue;
    }

    var tests = [];
    for (var test in spec.sections[section_name]) {
      if (!spec.sections[section_name].hasOwnProperty(test)) {
        continue;
      }
      var expected_ast = spec.sections[section_name][test];
      var options = spec.sections[section_name][test]['%parse_options%'] || {};
      delete expected_ast['%parse_options%'];
      tests.push({
        content: test,
        expected_ast: expected_ast,
        options: options,
      });
    }

    var section = {
      tests: tests,
    };

    sections[section_name] = {
      tests: tests,
    };
  }

  return sections;
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

    for (section_name in results.failures) {
      if (results.failures.hasOwnProperty(section_name)) {
        console.log("===%s Failures===".bold, section_name);
        for (test in results.failures[section_name]) {
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
