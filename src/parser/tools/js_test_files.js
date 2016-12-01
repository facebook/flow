#!/usr/bin/env node
var flow = require("../flow_parser.js");
var colors = require("colors");
var util = require("util");
var fs = require("fs");
var parseArgs = require("minimist");
var ast_types = require("./../test/esprima_ast_types.js");

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

var total_parse_time = 0;
var options = {
  esproposal_class_instance_fields: true,
  esproposal_class_static_fields: true,
  types: true,
};

function parse_file(filename, data) {
  var result;
  try {
    process.stdout.write(filename + " : ... ");
    var start_time = +new Date;
    var ast = flow.parse(data, options);
    var parse_time = (+new Date - start_time)/1000;
    total_parse_time += parse_time;
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
    var time_string = "[" + parse_time + "]";
    if (result.passed) {
      pass_count++;
      console.log("\rPASSED".green, time_string, filename);
    } else {
      fail_count++;
      if (argv.verbose) {
        var data = util.inspect(result.data, {depth: null});
        console.log("\rFAILED".red, time_string, filename);
        console.log("\t", result.error, "\n\t", data);
      } else {
        console.log("\rFAILED".red, time_string, filename, result.error);
        if (argv.firstError && result.data && result.data.length > 0) {
          console.log("\t", result.data[0]);
        }
      }
    }
    if (pass_count + fail_count == argv._.length) {
      console.log("%d/%d passed", pass_count, pass_count + fail_count);
      console.log("Total parsing time: %d", total_parse_time);
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
