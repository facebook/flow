/* @flow */

const child_process = require('child_process');
const fs = require('fs');
const stream = require('stream');
const ls = child_process.spawn('ls');

const data = 'foo';

ls.stdin.write(data);
ls.stdin.write(data, 'utf-8');
ls.stdin.write(data, () => {});
ls.stdin.write(data, 'utf-8', () => {});

ls.stdin.end();
ls.stdin.end(data);
ls.stdin.end(data, 'utf-8');
ls.stdin.end(data, () => {});
ls.stdin.end(data, 'utf-8', () => {});

const ws = fs.createWriteStream('/dev/null');
ls.stdout.pipe(ws).end();

class MyReadStream extends stream.Readable {}
class MyWriteStream extends stream.Writable {}
class MyDuplex extends stream.Duplex {}
class MyTransform extends stream.Duplex {}

const pipe = new MyReadStream()
  .pipe(new MyDuplex())
  .pipe(new MyTransform())
  .pipe(new MyWriteStream());

(pipe: stream.Writable);
(pipe: MyWriteStream);
(pipe: MyDuplex); // error

const pipeline = stream.pipeline(
  new MyReadStream(),
  new MyDuplex(),
  new MyTransform(),
  new MyWriteStream(),
  error => {
    (error: ?Error);
    (error: null); // error
  },
);

(pipeline: MyWriteStream);
(pipeline: MyDuplex); // error

stream.pipeline(
  new MyWriteStream(), // error - first stream must be Readable
  new MyDuplex(),
  () => {},
);

stream.pipeline(
  new MyDuplex(),
  new MyWriteStream(), // error - middle stream must be Duplex
  new MyDuplex(),
  () => {},
);

stream.pipeline(
  new MyDuplex(),
  new MyDuplex(),
  new MyReadStream(), // error - last stream must be Writable
  () => {},
);

new MyReadStream()
  .on('error', () => {})
  .pipe(new MyDuplex())
  .once('close', () => {});


async function * generate() {
  yield 'hello';
  yield 'streams';
}

stream.Readable.from(generate());

stream.Readable.from('banana');
stream.Readable.from(101); // error - TypeError [ERR_INVALID_ARG_TYPE]: The "iterable" argument must be an instance of Iterable. Received type number (101)
stream.Readable.from(null); // error - TypeError [ERR_INVALID_ARG_TYPE]: The "iterable" argument must be an instance of Iterable. Received null
