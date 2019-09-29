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
    ast_types_error: function(msg) {
      var p = path[0];
      pathsWithAstTypesErrors[p] = true;
      delete astTypesErrors[p];
      for (var i = 1; i < path.length; i++) {
        p += "." + path[i];
        pathsWithAstTypesErrors[p] = true;
        delete astTypesErrors[p];
      }
      var full_path = path.join(".");
      astTypesErrors[full_path] = msg;
    },

    should_run_ast_types: function() {
      var p = path.join(".");
      return pathsWithAstTypesErrors[p] !== true;
    },

    get_diffs: function() {
      var ret = [];
      for (var prop in diffs) {
        if (diffs.hasOwnProperty(prop)) {
          ret.push(diffs[prop]);
        }
      }
      return ret;
    },

    get_ast_types_errors: function() {
      var ret = [];
      for (var prop in astTypesErrors) {
        if (astTypesErrors.hasOwnProperty(prop)) {
          ret.push(prop + ": " + astTypesErrors[prop]);
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
      ". Expected " + JSON.stringify(diff.expected) +
      ", got " + JSON.stringify(diff.actual);
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
    }
    if (ast.type && env.should_run_ast_types()) {
      if (ast_types.namedTypes.hasOwnProperty(ast.type)) {
        try {
          ast_types.namedTypes[ast.type].assert(ast, true);
        } catch (e) {
          env.ast_types_error(e.message);
        }
      } else {
        env.ast_types_error('Unknown type ' + ast.type);
      }
    }
  }
}

function compare(env, ast, spec, skip_comments) {
  if (Array.isArray(spec)) {
    if (Array.isArray(ast)) {
      if (spec.length != ast.length) {
        env.diff("Array has wrong size", spec.length, ast.length);
      }
      for (var i = 0; i < spec.length; i++) {
        if (spec.hasOwnProperty(i)) {
          if (ast.hasOwnProperty(i)) {
            env.push_path(i);
            compare(env, ast[i], spec[i], skip_comments);
            env.pop_path();
          }
        }
      }
    } else {
      env.diff("Not an array");
    }
  } else if (spec != null && typeof spec == "object") {
    for (var prop in spec) {
      var is_comments_prop =
        prop === "trailingComments" || prop === "leadingComments";
      if (skip_comments && is_comments_prop) {
        continue;
      }
      if (spec.hasOwnProperty(prop)) {
        var path = prop.split(".");
        var sub_ast = ast;
        var found = true;
        var i;
        for (i = 0; i < path.length; i++) {
          var pathProp = path[i];
          if (sub_ast && sub_ast.hasOwnProperty(pathProp)) {
            sub_ast = sub_ast[pathProp];
          } else {
            env.diff('Missing property "'+pathProp+'"');
            found = false;
            break;
          }
          env.push_path(pathProp);
        }
        if (found) {
          compare(env, sub_ast, spec[prop], skip_comments);
        }
        for (; i > 0; i--) {
          env.pop_path();
        }
      }
    }
  } else {
    if (ast !== spec && !(ast instanceof RegExp && spec === null)) {
      env.diff("Wrong value", spec, ast);
    }
  }
}

function runTest(test, parse_options, test_options) {
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
  try {
    var flow_ast = test_options.flow.parse(test.content, parse_options);
  } catch (e) {
    output("Flow exploded:", util.inspect(e, {depth: null}));
    result.passed = false;
    return result;
  }
  if (test_options.dumpAst) {
    console.log("AST: ", util.inspect(flow_ast, {depth: null, colors: true}));
    output("AST: ", util.inspect(flow_ast, {depth: null, colors: true}));
  }
  var env = new_env();
  var flow_errors = flow_ast.errors;
  if (!flow_errors || flow_errors.length == 0) {
    // parse errors can lead to an invalid AST, so only validate the AST when
    // there are no parse errors.
    check_ast(env, flow_ast);
  }
  compare(env, flow_ast, test.expected_ast, true);

  var diffs = env.get_diffs();
  if (test_options.jsonErrors) {
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

  var expected_to_error = test.expected_ast.hasOwnProperty('errors') && test.expected_ast.errors != [];
  for (var prop in test.expected_ast) {
    if (test.expected_ast.hasOwnProperty(prop) && prop.match(/^errors\./)) {
      expected_to_error = true;
      break;
    }
  }

  if (flow_errors.length !== 0 && !expected_to_error) {
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
