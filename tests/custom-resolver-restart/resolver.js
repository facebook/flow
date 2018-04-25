#!/usr/bin/env node

const fs = require('fs');
const StringDecoder = require('string_decoder');

let buffer = '';
const decoder = new StringDecoder.StringDecoder();

let ext = `foo`;

process.stdin.on('data', chunk => {
  buffer += decoder.write(chunk);

  do {
    const index = buffer.indexOf('\n');
    if (index === -1) {
      break;
    }

    const line = buffer.slice(0, index);
    buffer = buffer.slice(index + 1);

    try {
      let path = `${__dirname}/${JSON.parse(line)[0]}.${ext}.js`;
      if (fs.existsSync(path)) {
        process.stdout.write(`${JSON.stringify([null, path])}\n`);
      } else {
        process.stdout.write(`${JSON.stringify([`File not found`, null])}\n`);
      }
    } catch (error) {
      process.stdout.write(`${JSON.stringify([error.message, null])}\n`);
    }
  } while (true);
});
