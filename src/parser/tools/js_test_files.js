#!/usr/bin/env node
var flow = require("../flow_parser.js");
var colors = require("colors");
var util = require("util");
var fs = require("fs");
var parseArgs = require("minimist");
var ast_types = require("ast-types");

var argv = parseArgs(
  process.argv.slice(2),
  {
    alias: {verbose: "v"},
    boolean: [ "verbose", "v", "firstError" ]
  }
);

var pass_count = 0;
var fail_count = 0;

function error(e, data) {
  return {
    passed: false,
    error: e,
    data: data
  };
}
function success(e) {
  return {
    passed: true
  }
}

function parse_file(filename, data) {
  var result;
  try {
    process.stdout.write(filename + " : ... ");
    var ast = flow.parse(data, {});
    if (ast.errors.length > 0) {
      result = error("Parse errors!", ast.errors);
      return;
    }
    if (!ast_types.namedTypes.Program.check(ast, true)) {
      result = error("Bad AST");
      return;
    }
    result = success();
  } catch (e) {
    var data = {
      message: e.toString(),
      stack: e.stack
    }
    result = error("Flow exploded", data);
  } finally {
    if (result.passed) {
      pass_count++;
      console.log("\rPASSED".green, filename);
    } else {
      fail_count++;
      if (argv.verbose) {
        var data = util.inspect(result.data, {depth: null});
        console.log("\rFAILED".red, filename);
        console.log("\t", result.error, "\n\t", data);
      } else {
        console.log("\rFAILED".red, filename, result.error);
        if (argv.firstError && result.data && result.data.length > 0) {
          console.log("\t", result.data[0]);
        }
      }
    }
    if (pass_count + fail_count == argv._.length) {
      console.log("%d/%d passed", pass_count, pass_count + fail_count);
    }
  }
}

if (argv.help || argv._.length == 0) {
  console.log("Tests 1 or more files to see if they parse successfully and if "+
              "the produced ASTs are valid according to ast-types");
  console.log("Usage: ./js_test_files [file1] [file2] ... [filen]");
  console.log("\nOptions:");
  var options = {
    "--verbose": "For each failure, print all relevant information",
    "--firstError": "For each failure, print only the first error"
  }
  for (prop in options) {
    if (options.hasOwnProperty(prop)) {
      console.log("\t%s\t%s", prop, options[prop]);
    }
  }
}

for (var i = 0; i < argv._.length; i++) {
  var data = fs.readFileSync(argv._[i] + "").toString();
  parse_file(argv._[i], data);
}
