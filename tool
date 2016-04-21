#!/usr/bin/env node

/* @flow */

process.stderr.write("Starting babel transform...\n");

require("babel-register");
require('babel-polyfill');
require("./tsrc/main.js").run();
