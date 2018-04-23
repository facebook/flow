#!/usr/bin/env node

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
      const data = JSON.parse(line);
      if (data[0] === `string_decoder`) {
        process.stdout.write(`${data[0]}\n`);
      } else {
        process.stdout.write(`${__dirname}/${data[0]}.${ext}.js\n`);
      }
    } catch (error) {
      process.stdout.write(`${error.message}\n`);
    }
  } while (true);
});
