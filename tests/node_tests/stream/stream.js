/* @flow */

var child_process = require('child_process');
var ls = child_process.spawn('ls');

var data = "foo";

ls.stdin.write();
ls.stdin.write(data);
ls.stdin.write(data, "utf-8");
ls.stdin.write(data, () => {});
ls.stdin.write(data, "utf-8", () => {});

ls.stdin.end();
ls.stdin.end(data);
ls.stdin.end(data, "utf-8");
ls.stdin.end(data, () => {});
ls.stdin.end(data, "utf-8", () => {});
