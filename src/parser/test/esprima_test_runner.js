/**
 * Compares the output of flow_parser.js with esprima
 */

var esprima = require("esprima-fb");
var flow = require("../flow_parser.js");
var util = require("util");
var ast_types = require("./esprima_ast_types.js");

function new_env() {
  var diffs = {};
  var astTypesErrors = [];
  var pathsWithAstTypesErrors = {};
  var path = ["root"];
  return {
    push_path: path.push.bind(path),
    pop_path: path.pop.bind(path),
    path_level: function () { return path.length; },

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
      var full_path = path.join(".");
      astTypesErrors.push(full_path);
      var p = path[0];
      pathsWithAstTypesErrors[p] = true;
      for (var i = 1; i < path.length; i++) {
        p += "." + path[i];
        pathsWithAstTypesErrors[p] = true;
      }
    },

    should_run_ast_types: function() {
      var p = path.join(".");
      return pathsWithAstTypesErrors[p] !== true;
    },

    get_diffs: function() { return diffs; },

    get_ast_types_errors: function() {
      return astTypesErrors;
    }
  };
}

function diff_to_string(diff) {
  var expected_str = "";
  if (typeof diff.expected !== "undefined" ||
      typeof diff.actual !== "undefined") {
    expected_str =
      ". Expected " + diff.expected + ", got " + diff.actual;
  }
  return diff.path + ": " + diff.type + expected_str;
}

function handleSpecialObjectCompare(esprima, flow, env) {
  // Source location special handling
  if (esprima.hasOwnProperty("start") && esprima.hasOwnProperty("end")) {
    // Esprima omits the source property
    if (!esprima.hasOwnProperty("source") && flow.source === null) {
      esprima.source = null;
    }
  }

  /** TYPE ANNOTATION COMPATIBILITY **/
  if (esprima.hasOwnProperty("typeAnnotation") &&
      typeof esprima.typeAnnotation == "undefined") {
    esprima.typeAnnotation = null;
  }

  // Root object
  if (env.path_level() == 1) {
    // Ignore the errors property on flow
    delete flow.errors;
  }

  switch (esprima.type) {
    case "SwitchStatement":
      // Esprima doesn't support let statements so it doesn't include the
      // lexical field
      delete flow.lexical;
      break;
    case 'TryStatement':
      // The Mozilla spec changed over time. There used to be a "handlers"
      // property with a list of catch clauses. Now there is just a single
      // "handler" property. Esprima still only supports the old spec. Flow
      // follows the new spec
      flow.handlers = flow.handler ? [ flow.handler ] : [];
      delete flow.handler;
      break;
    case 'CatchClause':
      // Esprima doesn't support the guard property for catch clauses
      if (!esprima.hasOwnProperty("guard")) {
        esprima.guard = null;
      }
      break;
    case 'Identifier':
      if (esprima.optional === undefined) {
        esprima.optional = false;
      }
      break;
    case 'FunctionDeclaration':
    case 'FunctionExpression':
      // Some more type annotation stuff missing from esprima
      if (esprima.typeParameters === undefined) {
        esprima.typeParameters = null;
      }
      if (esprima.returnType === undefined) {
        esprima.returnType = null;
      }
      break;
    case 'JSXEmptyExpression':
      // The location for the empty JSX expression doesn't really matter. I'm
      // arbitrarily using the location of the {}, and esprima is arbitrarily
      // using the single column location immediately after the {}
      esprima.loc = flow.loc;
      break;
    case 'ObjectPattern':
      // ObjectPattern should contain a list of PropertyPattern nodes, not
      // Property nodes. These nodes are clearly just patterns and don't
      // contain a lot of fields that don't make sense in a pattern
      for (var i = 0; i < esprima.properties.length; i++) {
        var prop = esprima.properties[i];
        switch (prop.type) {
          case 'SpreadProperty':
            prop.type = 'SpreadPropertyPattern';
            break;
          case 'Property':
            prop.type = 'PropertyPattern';
            prop.pattern = prop.value;
            delete prop.value;
            delete prop.kind;
            delete prop.method;
            delete prop.shorthand;
            break;
        }
      }
      if (!esprima.hasOwnProperty('typeAnnotation')) {
        esprima.typeAnnotation = null;
      }
      break;
    case 'MethodDefinition':
      // Esprima uses "get"/"set"/"" instead of "get"/"set"/"init"
      if (esprima.kind == "") {
        esprima.kind = "init";
      }
      break;
    case 'ClassExpression':
      // Should use null for missing node
      if (esprima.id === undefined) {
        esprima.id = null;
      }
      if (esprima.typeParameters === undefined) {
        esprima.typeParameters = null;
      }
      if (esprima.superTypeParameters === undefined) {
        esprima.superTypeParameters = null;
      }
      if (esprima.implements === undefined) {
        esprima.implements = [];
      }
      break;
    case "ClassDeclaration":
      if (esprima.typeParameters === undefined) {
        esprima.typeParameters = null;
      }
      if (esprima.superTypeParameters === undefined) {
        esprima.superTypeParameters = null;
      }
      if (esprima.implements === undefined) {
        esprima.implements = [];
      }
      break;
    case "ClassBody":
      // esprima-fb is pretty out of date here. The 4 kinds should be
      // "constructor", "method", "get" and "set"
      for (var i = 0; i < esprima.body.length; i++) {
        var body = esprima.body[i];
        if (body && body.type == "MethodDefinition") {
          if (body.key.name === "constructor") {
            body.kind = "constructor";
          } else if (body.kind === "init" || body.kind === "") {
            body.kind = "method";
          }
        }
      }
      break;
    case 'ArrayPattern':
      // Esprima has the wrong node type for spread elements in an array pattern
      for (var i = 0; i < esprima.elements.length; i++) {
        if (esprima.elements[i] && esprima.elements[i].type == "SpreadElement") {
          esprima.elements[i].type = "SpreadElementPattern";
        }
      }
      if (!esprima.hasOwnProperty('typeAnnotation')) {
        esprima.typeAnnotation = null;
      }
      break;
    case 'ObjectTypeProperty':
    case 'ObjectTypeIndexer':
    case 'ObjectTypeCallProperty':
      esprima.static = esprima.static || false;
      break;
    case 'ClassProperty':
      esprima.static = esprima.static || false;
      esprima.value = null;
      break;
    case 'ArrowFunctionExpression':
      esprima.returnType = null;
      esprima.typeParameters = null;
      break;
  }

  switch (esprima.type) {
    case 'FunctionDeclaration':
    case 'FunctionExpression':
    case 'ArrowFunctionExpression':
      if (Array.isArray(esprima.defaults)) {
        for (var i = 0; i < esprima.defaults.length; i++) {
          if (esprima.defaults[i] === undefined) {
            esprima.defaults[i] = null;
          }
        }
      }
      if (esprima.async === undefined) {
        esprima.async = false;
      }
  }

  if (flow && flow.type) {
    switch (flow.type) {
      case "JSXText":
        // Esprima represents JSX children string literals as Literal nodes
        flow.type = "Literal";
        break;
    }
  }
}

function compare(esprima, flow, env) {
  var esprima_type = typeof esprima;
  var flow_type = typeof flow;
  if (esprima_type === "object") {
    if (esprima === null) {
      esprima_type = "null";
    } else if (Array.isArray(esprima)) {
      esprima_type = "Array";
    }
  }
  if (flow_type === "object") {
    if (flow === null) {
      flow_type = "null";
    } else if (Array.isArray(flow)) {
      flow_type = "Array";
    }
  }

  if (esprima_type != flow_type) {
    env.diff("Wrong type", esprima_type, flow_type);
    return;
  }
  if (esprima_type == "string") {
    if (esprima !== flow) {
      env.diff("Wrong string", esprima, flow);
    }
  } else if (esprima_type == "number") {
    if (esprima !== flow) {
      env.diff("Wrong number", esprima, flow);
    }
  } else if (esprima_type == "boolean") {
    if (esprima !== flow) {
      env.diff("Wrong boolean", esprima, flow);
    }
  } else if (esprima_type == "Array") {
    if (esprima.length !== flow.length) {
      env.diff("Wrong array length", esprima.length, flow.length);
    } else {
      for (var i = 0; i < esprima.length; i++) {
        env.push_path(i);
        compare(esprima[i], flow[i], env);
        env.pop_path();
      }
    }
  } else if (esprima === null) {
    if (esprima !== flow) {
      env.diff("Wrong null", esprima, flow);
    }
  } else if (esprima instanceof RegExp) {
    if (!flow instanceof RegExp) {
      env.diff("type", "RegExp", flow_type);
      return;
    }
    if (String(esprima) != String(flow)) {
      env.diff("Wrong RegExp", String(esprima), String(flow));
      return;
    }
  } else if (esprima_type == "object") {
    handleSpecialObjectCompare(esprima, flow, env);
    for (prop in esprima) {
      if (!esprima.hasOwnProperty(prop)) {
        continue;
      }
      env.push_path(prop);
      if (!flow || !flow.hasOwnProperty(prop)) {
        env.diff("Missing property");
      } else {
        compare(esprima[prop], flow[prop], env);
      }
      env.pop_path();
    }
    for (prop in flow) {
      if (flow.hasOwnProperty(prop) && !esprima.hasOwnProperty(prop)) {
        env.push_path(prop);
        env.diff("Unexpected property");
        env.pop_path();
      }
    }
  }

  if (flow && flow_type == "object" && flow.hasOwnProperty("type") ) {
    if (env.should_run_ast_types() &&
        !ast_types.namedTypes[flow.type].check(flow, true)) {
      env.ast_types_error();
    }
  }
}

function runTest(test, esprima_opts) {
  var env = new_env();
  var comparing_errors = false;
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
    var flow_ast = flow.parse(test.content);
  } catch (e) {
    output("Flow exploded:", util.inspect(e, {depth: null}));
    result.passed = false;
    return result;
  }
  try {
    var options = { loc: true, comment: true, range: true };
    for (opt in esprima_opts) {
      options[opt] = esprima_opts[opt];
    }
    var esprima_ast = esprima.parse(test.content, options);
  } catch (e) {
    comparing_errors = true;
    if (test.dumpAst) {
      console.log("Esprima AST: ", util.inspect(e, {depth: null, colors: true}));
      console.log("Flow AST: ", util.inspect(flow_ast, {depth: null, colors: true}));
      output("Esprima AST: ", util.inspect(e, {depth: null, colors: true}));
      output("Flow AST: ", util.inspect(flow_ast, {depth: null, colors: true}));
    }

    var errors = flow_ast.errors || [];
    env.push_path('errors');
    if (errors.length > 0) {
      // Errors seem to be 1-indexed for columns in esprima's errors
      env.push_path('0');
      if (errors[0].loc.start.column > e.column - 1 || e.column - 1 > errors[0].loc.end.column) {
        env.push_path('column');
        env.diff("Wrong error column", e.column - 1, errors[0].loc.start.column + "-" + errors[0].loc.end.column);
        env.pop_path();
      }
      if (errors[0].loc.start.line > e.lineNumber || e.lineNumber > errors[0].loc.end.line) {
        env.push_path('line');
        env.diff("Wrong error line", e.lineNumber, errors[0].loc.start.line + "-" + errors[0].loc.end.line);
        env.pop_path();
      }
      if (errors[0].message != e.description) {
        env.push_path('message');
        env.diff("Wrong error message", e.description, errors[0].message);
        env.pop_path();
      }
      env.pop_path();
    } else{
      env.diff("Flow found no error", e.message);
    }
    env.pop_path();
  }

  // Some checks that we only run when comparing asts
  if (!comparing_errors) {
    if (test.dumpAst) {
      console.log("Esprima AST: ", util.inspect(esprima_ast, {depth: null, colors: true}));
      console.log("Flow AST: ", util.inspect(flow_ast, {depth: null, colors: true}));
      output("Esprima AST: ", util.inspect(esprima_ast, {depth: null, colors: true}));
      output("Flow AST: ", util.inspect(flow_ast, {depth: null, colors: true}));
    }
    var flow_errors = flow_ast.errors;
    compare(esprima_ast, flow_ast, env);

    if (flow_errors.length !== 0) {
      env.push_path('errors');
      for (var i = 0; i < flow_errors.length; i++) {
        var e = flow_errors[i];
        env.push_path(i);
        env.push_path('column');
        env.diff("Wrong error column", undefined, e.loc.start.column + 1);
        env.pop_path();
        env.push_path('line');
        env.diff("Wrong error line", undefined, e.loc.start.line);
        env.pop_path();
        env.push_path('message');
        env.diff("Wrong error message", undefined, e.message);
        env.pop_path();
      }
      env.pop_path();
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
  }

  if (test.showDifferences) {
    test.expected_differences = {};
    output(test.explanation || "No explanation provided");
  }

  var actual = env.get_diffs();
  var expected = test.expected_differences || {};
  var missing_diffs = [];
  var unexpected_differences = [];
  var mismatched_diffs = [];
  for (path in expected) {
    if (!expected.hasOwnProperty(path)) {
      continue;
    }

    if (path in actual) {
      if (actual[path].type != expected[path].type ||
          actual[path].expected != expected[path].expected ||
          actual[path].actual != expected[path].actual) {
        expected[path].path = path;
        mismatched_diffs.push({expected: expected[path], actual: actual[path]});
      }
    } else {
      expected[path].path = path;
      missing_diffs.push(expected[path]);
    }
  }

  for (path in actual) {
    if (actual.hasOwnProperty(path) && !expected.hasOwnProperty(path)) {
      unexpected_differences.push(actual[path]);
    }
  }

  if (test.jsonErrors) {
    diff_to_string = function (e) { return util.inspect(e, { depth: null }); };
  }
  if (missing_diffs.length > 0) {
    result.passed = false;
    output("****Missing Expected Differences****");
    for (var i = 0; i < missing_diffs.length; i++) {
      output("(#" + i + ")", diff_to_string(missing_diffs[i]));
    }
  }
  if (unexpected_differences.length > 0) {
    result.passed = false;
    output("****Unexpected Differences****");
    for (var i = 0; i < unexpected_differences.length; i++) {
      output("(#" + i + ")", diff_to_string(unexpected_differences[i]));
    }
  }
  if (mismatched_diffs.length > 0) {
    result.passed = false;
    output("****Mismatched Differences****");
    for (var i = 0; i < mismatched_diffs.length; i++) {
      output(
        "(#" + i + ") Expected ",
        diff_to_string(mismatched_diffs[i].expected)
      );
      output(
        "(#" + i + ") Actual ",
        diff_to_string(mismatched_diffs[i].actual)
      );
    }
  }
  return result;
}

module.exports = runTest;
