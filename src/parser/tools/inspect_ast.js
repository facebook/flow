#!/usr/bin/env node
var flow = require("../flow_parser.js");
var util = require("util");
var fs = require("fs");
var argv = require("minimist")(
  process.argv.slice(2),
  {
    boolean: [
      "strip-comments"
    ],
  }
);

var description =
"This script parses a given string or file and then dumps the ast.\n\
You can either pass in a string to parse like\n\
\t./inspect_ast.js \"1+1\"\n\
Or you can pass in the name of a file to parse, like \n\
\t./inspect_ast.js inspect_ast.js";
if (argv._.length != 1 || argv.help) {
  console.log(description);
} else {
  var content = argv._[0];
  if (fs.existsSync(content)) {
    content = fs.readFileSync(content).toString();
  }
  var ast = flow.parse(content, {});
  if (argv['strip-comments']) {
    delete ast.comments;
  }
  console.log(JSON.stringify(ast, null, 2));
}
