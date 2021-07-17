// @flow

const { Console } = require("console");
const stream = require("stream");

const myConsole = new Console(new stream.Writable());

// Instances of Console should be compatible with the global console.
(myConsole: typeof console);

const consoleOptions = new Console({
  stdout: new stream.Writable(),
  stderr: new stream.Writable(),
  ignoreErrors: false,
  colorMode: "auto",
  inspectOptions: {
    showHidden: true,
    depth: 42,
    colors: false,
    customInspect: false
  },
  groupIndentation: 42
});

const noNew = Console(new stream.Writable(), new stream.Writable(), true);
