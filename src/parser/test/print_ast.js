#!/usr/bin/env node

/*
 * `cat FILE | print_ast.js [OPTIONS]`
 *
 * Print the AST of JavaScript file named FILE to stdout. An optional file
 * named OPTIONS provides JSON formatted options to the Flow parser. See
 * `flow.org/en/docs/config/options/#toc-available-options` for available
 * options.
 */

const parser = require("../flow_parser");
const fs = require("fs");

const arg = process.argv[2]
const options = (arg && JSON.parse(fs.readFileSync(process.cwd() + "/" + arg))) || {};

const stdin = process.stdin;
const stdout = process.stdout;
const stderr = process.stderr;

let text = "";

let js = new Promise((resolve, reject) => {
  stdin.setEncoding("utf8");

  stdin.on("readable", () => {
    let chunk;

    while ((chunk = stdin.read())) {
      text += chunk;
    }
  });

  stdin.on("end", () => {
    resolve(text);
  });
});

function pretty_print(key, value) {
  switch (key) {
  case "loc":
  case "range":
    return JSON.stringify(value);
  default:
    return value;
  }
}

function process_line(line) {
  const unescaped = /(^)"|([^\\])"/g;
  const escaped = /\\\"/g;
  if (line.match(/^ *\"loc\":|^ *\"range\":/)) {
    const parts = line.split(":");
    const key = parts.shift();
    const value = parts.join(":");
    return key + ":" + value.replace(unescaped, "$2").replace(escaped, "\"");
  } else {
    return line;
  }
}

js.then(text => JSON.stringify(parser.parse(text, options), pretty_print, 2))
  .then(text => text.replace(/\": /g, "\":"))
  .then(text => text.split("\n").map(process_line).join("\n"))
  .then(text => stdout.write(text + "\n"))
  .catch(err => stderr.write(err + "\n"));
