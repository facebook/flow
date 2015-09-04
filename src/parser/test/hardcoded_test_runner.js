var flow = require("./../flow_parser.js");
var util = require("util");
var ast_types = require("./esprima_ast_types.js");

function new_env() {
  var diffs = {};
  var astTypesErrors = {};
  var pathsWithAstTypesErrors = {};
  var path = ["root"];
  return {
    push_path: path.push.bind(path),
    pop_path: path.pop.bind(path),

    diff: function(type, expected, actual) {
      var path_str = path.join(".");
      diffs[path_str] = {
        path: path_str,
        type: type,
        expected: expected,
        actual: actual
      };
    },

    /** If we see an ast types error at root.foo.bar then we record the
     * subpaths so that we don't rerun the ast types recursive check for root
     * or root.foo
     */
    ast_types_error: function() {
      var p = path[0];
      pathsWithAstTypesErrors[p] = true;
      delete astTypesErrors[p];
      for (var i = 1; i < path.length; i++) {
        p += "." + path[i];
        pathsWithAstTypesErrors[p] = true;
        delete astTypesErrors[p];
      }
      var full_path = path.join(".");
      astTypesErrors[full_path] = true;
    },

    should_run_ast_types: function() {
      var p = path.join(".");
      return pathsWithAstTypesErrors[p] !== true;
    },

    get_diffs: function() {
      var ret = [];
      for (prop in diffs) {
        if (diffs.hasOwnProperty(prop)) {
          ret.push(diffs[prop]);
        }
      }
      return ret;
    },

    get_ast_types_errors: function() {
      var ret = [];
      for (prop in astTypesErrors) {
        if (astTypesErrors.hasOwnProperty(prop)) {
          ret.push(prop);
        }
      }
      return ret;
    }
  };
}

function diff_to_string(diff) {
  var expected_str = "";
  if (typeof diff.expected !== "undefined") {
    expected_str =
      ". Expected " + diff.expected + ", got " + diff.actual;
  }
  return diff.path + ": " + diff.type + expected_str;
}

function check_ast(env, ast) {
  if (ast && typeof ast == "object") {
    for (var prop in ast) {
      if (!ast.hasOwnProperty(prop)) {
        continue;
      }
      env.push_path(prop);
      check_ast(env, ast[prop]);
      env.pop_path();
      if (ast.type && env.should_run_ast_types()) {
        if (ast_types.namedTypes.hasOwnProperty(ast.type)) {
          if (!ast_types.namedTypes[ast.type].check(ast, true)) {
            env.ast_types_error();
          }
        } else {
          env.ast_types_error();
        }
      }
    }
  }
}

function compare(env, ast, spec) {
  if (Array.isArray(spec)) {
    if (Array.isArray(ast)) {
      if (spec.length != ast.length) {
        env.diff("Array has wrong size", spec.length, ast.length);
      }
      for (var i = 0; i < spec.length; i++) {
        if (spec.hasOwnProperty(i)) {
          if (ast.hasOwnProperty(i)) {
            env.push_path(i);
            compare(env, ast[i], spec[i]);
            env.pop_path();
          }
        }
      }
    } else {
      env.diff("Not an array");
    }
  } else if (spec != null && typeof spec == "object") {
    for (prop in spec) {
      if (spec.hasOwnProperty(prop)) {
        var path = prop.split(".");
        var sub_ast = ast;
        var found = true;
        var i;
        for (i = 0; i < path.length; i++) {
          if (sub_ast && sub_ast.hasOwnProperty(path[i])) {
            sub_ast = sub_ast[path[i]];
          } else {
            env.diff('Missing property "'+path[i]+'"');
            found = false;
            break;
          }
          env.push_path(path[i]);
        }
        if (found) {
          compare(env, sub_ast, spec[prop]);
        }
        for (; i > 0; i--) {
          env.pop_path();
        }
      }
    }
  } else {
    if (ast !== spec) {
      env.diff("Wrong value", spec, ast);
    }
  }
}

function runTest(test) {
  var result = {
    passed: true,
    output: ""
  };
  function output() {
    for (var i = 0; i < arguments.length; i++) {
      result.output += arguments[i] + " ";
    }
    result.output += "\n";
  }
  var parseOptions = test.parseOptions || {};
  try {
    var flow_ast = flow.parse(test.content, parseOptions);
  } catch (e) {
    output("Flow exploded:", util.inspect(e, {depth: null}));
    result.passed = false;
    return result;
  }
  if (test.dumpAst) {
    console.log("AST: ", util.inspect(flow_ast, {depth: null, colors: true}));
    output("AST: ", util.inspect(flow_ast, {depth: null, colors: true}));
  }
  var env = new_env();
  var flow_errors = flow_ast.errors;
  check_ast(env, flow_ast);
  compare(env, flow_ast, test.spec);

  var diffs = env.get_diffs();
  if (test.jsonErrors) {
    diff_to_string = function (e) { return util.inspect(e, { depth: null }); };
  }
  if (diffs.length > 0) {
    result.passed = false;
    output("****Unexpected Differences****");
    for (var i = 0; i < diffs.length; i++) {
      output("(#" + i + ")", diff_to_string(diffs[i]));
    }
  }

  var ast_types_errors = env.get_ast_types_errors();
  if (ast_types_errors.length !== 0) {
    result.passed = false;
    output("****AST Types Errors****");
    for (var i = 0; i < ast_types_errors.length; i++) {
      output(
        "(#" + i + ")",
        ast_types_errors[i]
      );
    }
  }

  if (flow_errors.length !== 0 && !test.spec.hasOwnProperty('errors')) {
    result.passed = false;
    output("****Unexpected Errors****");
    for (var i = 0; i < flow_errors.length; i++) {
      var e = flow_errors[i];
      output(
        "(#" + i + ")",
        "(line " + e.loc.start.line + ", col " + e.loc.start.column + ") - " +
        "(line " + e.loc.end.line + ", col " + e.loc.end.column + "): ",
        e.message
      );
    }
  }
  return result;
}

module.exports = runTest;
