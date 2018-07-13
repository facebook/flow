#!/usr/bin/env node

var fs = require('fs');
var StringDecoder = require('string_decoder');

var buffer = '';
var decoder = new StringDecoder.StringDecoder();

var ext = 'foo';

process.stdin.on('data', function (chunk) {
  buffer += decoder.write(chunk);

  do {
    var index = buffer.indexOf('\n');
    if (index === -1) {
      break;
    }

    var line = buffer.slice(0, index);
    buffer = buffer.slice(index + 1);

    try {
      var path = __dirname + '/' + JSON.parse(line)[0] + '.' + ext + '.js';
      if (fs.existsSync(path)) {
        process.stdout.write(JSON.stringify([null, path]) + '\n');
      } else {
        process.stdout.write(JSON.stringify([{
          message: `File not found`,
        }, null]) + '\n');
      }
    } catch (error) {
      process.stdout.write(JSON.stringify([{
        message: error.message,
      }, null]) + '\n');
    }
  } while (true);
});
