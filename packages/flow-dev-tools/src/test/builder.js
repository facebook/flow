/**
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {execSync, spawn} from 'child_process';
import {randomBytes} from 'crypto';
import {createWriteStream} from 'fs';
import {platform, tmpdir} from 'os';
import {basename, dirname, extname, join, sep as dir_sep} from 'path';
import {format} from 'util';
import EventEmitter from 'events';

import * as rpc from 'vscode-jsonrpc';

import type {IDEMessage, RpcConnection} from './ide';

import {
  appendFile,
  exec,
  execManual,
  isRunning,
  mkdirp,
  readdir,
  readFile,
  sleep,
  unlink,
  writeFile,
} from '../utils/async';
import {getTestsDir} from '../constants';

import type {SuiteResult} from './runTestSuite';

type CancellationToken = {
  +isCancellationRequested: boolean,
  onCancellationRequested(callback: () => any): void,
};

type CheckCommand = 'check' | 'status';

export class TestBuilder {
  bin: string;
  dir: string;
  errorCheckCommand: CheckCommand;
  flowConfigFilename: string;
  lazyMode: 'ide' | 'fs' | null;
  server: null | child_process$ChildProcess = null;
  ide: null | {
    mode: 'legacy' | 'lsp',
    connection: RpcConnection,
    process: child_process$ChildProcess,
    outstandingRequestsFromServer: Map<
      number,
      {|resolve: any => void, reject: Error => void|},
    >,
    outstandingRequestsInfo: {nextId: number, mostRecent: ?number},
    stderr: Array<string>,
    messageEmitter: EventEmitter,
  } = null;
  ideMessages: Array<IDEMessage>; // this should outlive the death of the ide+server in a step
  ideEmitter: EventEmitter;
  serverEmitter: EventEmitter;
  sourceDir: string;
  suiteName: string;
  tmpDir: string;
  testErrors = [];
  allowFlowServerToDie = false;
  logStream: stream$Writable | null;

  constructor(
    bin: string,
    errorCheckCommand: CheckCommand,
    baseDir: string,
    suiteName: string,
    testNum: number,
    flowConfigFilename: string,
    lazyMode: 'ide' | 'fs' | null,
  ) {
    this.bin = bin;
    // If we're testing lazy mode, then we must use status
    this.errorCheckCommand = lazyMode == null ? errorCheckCommand : 'status';
    this.suiteName = suiteName;
    this.ideEmitter = new EventEmitter();
    this.serverEmitter = new EventEmitter();
    this.dir = join(baseDir, String(testNum));
    this.sourceDir = join(getTestsDir(), suiteName);
    this.tmpDir = join(baseDir, 'tmp', String(testNum));
    this.flowConfigFilename = flowConfigFilename;
    this.lazyMode = lazyMode;
    this.ide = null;
    this.ideMessages = [];
  }

  getFileName(): string {
    return join(this.dir, 'test.js');
  }

  normalizeForFlowconfig(path: string): string {
    return path.split(dir_sep).join('/');
  }

  async log(fmt: string, ...args: Array<mixed>): Promise<void> {
    const {logStream} = this;
    if (logStream != null) {
      const now = new Date();
      function fixWidth(num: number, width: number): string {
        const str = String(num);
        return str.length < width ? '0'.repeat(width - str.length) + str : str;
      }
      const msg = format(
        '[%s %s:%s:%s.%s] %s\n',
        now.toLocaleDateString('en-US'),
        fixWidth(now.getHours(), 2),
        fixWidth(now.getMinutes(), 2),
        fixWidth(now.getSeconds(), 2),
        fixWidth(now.getMilliseconds(), 3),
        format(fmt, ...args),
      );
      return new Promise((resolve, reject) => {
        logStream.write(msg, 'utf8', resolve);
      });
    }
  }

  async closeLog(): Promise<void> {
    const {logStream} = this;
    if (logStream != null) {
      this.logStream = null;
      return new Promise((resolve, reject) => {
        logStream.end('', 'utf8', resolve);
      });
    }
  }

  async createFreshDir(): Promise<void> {
    await mkdirp(this.dir);
    await mkdirp(this.tmpDir);

    this.logStream = createWriteStream(join(this.tmpDir, 'test.log'));

    let configBuffer = null;
    const files = await readdir(this.sourceDir);
    for (const file of files) {
      if (basename(file) === this.flowConfigFilename) {
        configBuffer = await readFile(join(this.sourceDir, file));
      }
    }

    // We need to set the temp_dir option in the config. If there is no config,
    // that is easy. Otherwise we need to read the config. If temp_dir is
    // already set, then we default to that. Otherwise, we need to set it.
    if (configBuffer != null) {
      let config = configBuffer.toString().split('\n');
      let temp_dir = null;
      let options_index = null;
      config.forEach((line, index) => {
        const match = line.trim().match('^temp_dir=(.*)$');
        match != null && (temp_dir = match[1]);
        line.trim() == '[options]' && (options_index = index);
      });

      if (temp_dir == null) {
        if (options_index == null) {
          config.push('[options]');
          options_index = config.length - 1;
        }
        config.splice(
          options_index + 1,
          0,
          'temp_dir=' + this.normalizeForFlowconfig(this.tmpDir),
        );
      } else {
        this.tmpDir = temp_dir;
      }
      await writeFile(join(this.dir, '.flowconfig'), config.join('\n'));
    } else {
      await exec(
        format(
          '%s init --options "all=true;temp_dir=%s" %s',
          this.bin,
          this.normalizeForFlowconfig(this.tmpDir),
          this.dir,
        ),
        {cwd: __dirname},
      );
    }
    await writeFile(this.getFileName(), '/* @flow */\n');
  }

  async addCode(code: string): Promise<void> {
    const filename = this.getFileName();
    await appendFile(filename, '\n' + code + '\n');
    await this.forceRecheck([filename]);
  }

  async addFileImpl(source: string, dest: string): Promise<string> {
    source = join(this.sourceDir, source);
    dest = join(this.dir, dest);
    const contents_buffer = await readFile(source);
    let contents = contents_buffer.toString();
    if (contents.match(/@thisWillBeFlowInTest/)) {
      // Undo what the convert command did
      contents = contents.replace(/@thisWillBeFlowInTest/, '@flow');
    }
    await mkdirp(dirname(dest));
    await writeFile(dest, contents);
    return dest;
  }

  async addFile(source: string, dest: string): Promise<void> {
    const filename = await this.addFileImpl(source, dest);
    await this.forceRecheck([filename]);
  }

  async addFiles(sources: Array<string>): Promise<void> {
    const filenames = await Promise.all(
      sources.map(source => this.addFileImpl(source, source)),
    );
    await this.forceRecheck(filenames);
  }

  async removeFileImpl(file: string): Promise<string> {
    file = join(this.dir, file);
    await unlink(file);
    return file;
  }

  async removeFile(file: string): Promise<void> {
    const filename = await this.removeFileImpl(file);
    await this.forceRecheck([filename]);
  }

  async removeFiles(files: Array<string>): Promise<void> {
    const filenames = await Promise.all(
      files.map(file => this.removeFileImpl(file)),
    );
    await this.forceRecheck(filenames);
  }

  async modifyFile(
    filename: string,
    searchValue: string | RegExp,
    replaceValue: string,
  ): Promise<void> {
    filename = join(this.dir, filename);
    const buffer = await readFile(filename);
    const contents = buffer.toString().replace(searchValue, replaceValue);
    await writeFile(filename, contents);
    await this.forceRecheck([filename]);
  }

  async flowCmd(
    args: Array<string>,
    stdinFile?: string,
  ): Promise<[number, string, string]> {
    let cmd = format(
      '%s %s %s',
      this.bin,
      args.map(arg => format('"%s"', arg)).join(' '),
      stdinFile == null ? '' : format('< %s', stdinFile),
    );
    await this.log('# %s', cmd);
    const [err, stdout, stderr] = await execManual(cmd, {cwd: this.dir});
    const code = err == null ? 0 : err.code;

    return [code, stdout.toString(), stderr.toString()];
  }

  async getFlowErrors(retry?: boolean = true): Promise<Object> {
    let cmd;
    if (this.errorCheckCommand === 'check') {
      cmd = format(
        '%s check --strip-root --temp-dir %s --json %s',
        this.bin,
        this.tmpDir,
        this.dir,
      );
    } else {
      // No-op if it's already running
      await this.startFlowServer();
      cmd = format(
        '%s status --no-auto-start --strip-root --temp-dir %s --json %s',
        this.bin,
        this.tmpDir,
        this.dir,
      );
    }

    const [err, stdout, stderr] = await execManual(cmd, {
      cwd: __dirname,
      maxBuffer: 1024 * 1024,
    });

    // 0 - no errors
    // 2 - Some errors
    if (err == null || err.code === 2) {
      return JSON.parse(stdout.toString());
    }

    throw new Error(format('Flow check failed!', err, stdout, stderr));
  }

  async startFlowServer(): Promise<void> {
    if (this.server !== null) {
      return;
    }
    const lazyMode =
      this.lazyMode === null ? [] : ['--lazy-mode', this.lazyMode];
    const args = [
      'server',
      '--strip-root',
      '--debug',
      '--temp-dir',
      this.tmpDir,
      '--no-auto-restart',
      '--file-watcher',
      'none',
    ]
      .concat(lazyMode)
      .concat([this.dir]);
    await this.log('# %s %s', this.bin, args.join(' '));
    const serverProcess = spawn(this.bin, args, {
      // Useful for debugging flow server
      // stdio: ["pipe", "pipe", process.stderr],
      cwd: this.dir,
      env: {...process.env, OCAMLRUNPARAM: 'b'},
    });
    this.server = serverProcess;
    this.serverEmitter.emit('server');

    const stderr = [];
    serverProcess.stderr.on('data', data => {
      stderr.push(data.toString());
    });

    serverProcess.on('exit', (code, signal) => {
      if (this.server != null && !this.allowFlowServerToDie) {
        this.testErrors.push(
          format(
            'flow server mysteriously died. Code: %d, Signal: %s, stderr:\n%s',
            code,
            signal,
            stderr.join(''),
          ),
        );
      }
      this.stopFlowServer();
    });

    // Wait for the server to be ready or die
    await new Promise((resolve, reject) => {
      function done() {
        serverProcess.stderr.removeListener('data', resolveOnReady);
        serverProcess.stderr.removeListener('exit', resolveOnExit);
        resolve();
      }
      const resolveOnReady = data => {
        stderr
          .concat([data])
          .join('')
          .match(/Server is free/) && done();
      };
      const resolveOnExit = done;

      serverProcess.stderr.on('data', resolveOnReady);
      serverProcess.on('exit', resolveOnExit);
    });
  }

  stopFlowServer(): void {
    const server = this.server;
    if (server != null) {
      this.server = null;
      server.stdin.end();
      server.kill();
      this.serverEmitter.emit('server');
    }
  }

  async createIDEConnection(mode: 'legacy' | 'lsp'): Promise<void> {
    if (this.ide != null) {
      // No-op if the server is already running
      if (this.ide.mode == mode) {
        return;
      } else {
        throw new Error(`IDE process already running as ${this.ide.mode}.`);
      }
    }

    const primaryArg = mode === 'lsp' ? 'lsp' : 'ide';
    const args =
      mode === 'lsp'
        ? ['lsp', '--autostop', '--lazy-mode', 'ide']
        : [
            'ide',
            '--protocol',
            'very-unstable',
            '--no-auto-start',
            '--strip-root',
            '--temp-dir',
            this.tmpDir,
            '--root',
            this.dir,
          ];
    const ideProcess = spawn(this.bin, args, {
      // Useful for debugging flow ide
      // stdio: ["pipe", "pipe", process.stderr],
      cwd: this.dir,
      env: {...process.env, OCAMLRUNPARAM: 'b'},
    });
    const connection = rpc.createMessageConnection(
      new rpc.StreamMessageReader(ideProcess.stdout),
      new rpc.StreamMessageWriter(ideProcess.stdin),
    );
    connection.listen();

    // In nodejs, the 'exit' even is fired when the process exits;
    // the 'close' event is fired when its stdio streams have been closed.
    // The streams might be closed before exit if the process manually
    // closes them, or after if it leaves that to the system.
    ideProcess.on('exit', (code, signal) => {
      if (this.ide != null && this.ide.mode == 'legacy') {
        this.testErrors.push(
          format(
            'flow %s mysteriously died. Code: %d, Signal: %s, stderr:\n%s',
            primaryArg,
            code,
            signal,
            this.getIDEStderrSinceStartOfStep(),
          ),
        );
      }
      this.cleanupIDEConnection();
    });

    ideProcess.on('close', () => this.cleanupIDEConnection());

    const messageEmitter = new EventEmitter();
    const messages: Array<IDEMessage> = [];
    const outstandingRequestsInfo = {
      nextId: 1,
      mostRecent: (null: ?number),
    };
    const outstandingRequestsFromServer: Map<
      number,
      {|resolve: any => void, reject: Error => void|},
    > = new Map();

    connection.onRequest((method: string, ...rawParams: Array<mixed>) => {
      const id = outstandingRequestsInfo.nextId;
      outstandingRequestsInfo.mostRecent = id;
      outstandingRequestsInfo.nextId++;
      // the way vscode-jsonrpc works is the last element of the array is always
      // the cancellation token, and the actual params are the ones before it.
      const cancellationToken = ((rawParams.pop(): any): CancellationToken);
      // We'll add our own {id: ...} to the array of params, so it's present
      // in our messages[] array, so that people can match on it.
      const params = [{id}, ...this.sanitizeIncomingIDEMessage(rawParams)];
      messages.push({method, params});
      this.log('IDE <<request %s\n%s', method, JSON.stringify(params));
      messageEmitter.emit('message');

      cancellationToken.onCancellationRequested(() => {
        // The underlying Jsonrpc cancellation-request-notification has been
        // wrapped up by vscode-jsonrpc into a CancellationToken. We'll unwrap
        // it, for our messages[] array, so that tests can match on it.
        const synthesizedParams = [{id}];
        messages.push({method: '$/cancelRequest', params: synthesizedParams});
        this.log(
          'IDE <<notification $/cancelRequest\n%s',
          JSON.stringify(synthesizedParams),
        );
        messageEmitter.emit('message');
      });

      const promise = new Promise(
        (resolve: any => void, reject: Error => void) => {
          outstandingRequestsFromServer.set(id, {resolve, reject});
        },
      );
      return promise;
    });

    connection.onNotification((method: string, ...rawParams: Array<mixed>) => {
      const params = this.sanitizeIncomingIDEMessage(rawParams);
      messages.push({method, params});
      this.log('IDE <<notification %s\n%s', method, JSON.stringify(params));
      messageEmitter.emit('message');
    });

    const stderr = [];
    ideProcess.stderr.on('data', data => {
      stderr.push(data.toString());
    });

    await this.log('Created IDE process with pid %d', ideProcess.pid);

    // Execing a process can take some time. Let's wait for the ide process
    // to be up and connected to the server.
    const log = this.log.bind(this);
    await new Promise((resolve, reject) => {
      if (mode === 'lsp') {
        resolve();
        return;
      }
      const timeout = setTimeout(onTimeout, 20000); // Max 20 seconds
      function cleanup(then) {
        ideProcess.stderr.removeListener('data', onData);
        ideProcess.removeListener('exit', onExit);
        clearTimeout(timeout);
        then();
      }
      function onData(data) {
        stderr.join('').match(/Connected to server/) && cleanup(resolve);
      }
      function onExit() {
        cleanup(resolve);
      }
      function onTimeout() {
        log('flow ide start up timed out. stderr:\n%s', stderr.join('')).then(
          () => {
            cleanup(() => {
              reject(new Error('Timed out waiting for flow ide to start up'));
            });
          },
        );
      }
      ideProcess.stderr.on('data', onData);
      ideProcess.on('exit', onExit);
    });

    this.ideMessages = messages;
    this.ide = {
      mode,
      process: ideProcess,
      connection,
      outstandingRequestsFromServer,
      outstandingRequestsInfo,
      stderr,
      messageEmitter,
    };
    this.ideEmitter.emit('ide');
  }

  cleanupIDEConnection(): void {
    const ide = this.ide;
    if (ide != null) {
      this.ide = null;
      ide.process.stdin.end();
      ide.process.kill();
      ide.connection.dispose();
      this.ideEmitter.emit('ide');
      // but leave ideMessages so it can be examined even after IDE has gone
    }
  }

  getDirUrl(): string {
    if (platform() === 'win32') {
      return 'file:///' + this.dir;
    } else {
      return 'file://' + this.dir;
    }
  }

  // sanitizeIncomingIDEMessage: removes a few known fields from server output
  // that are known to be specific to an instance of a test, and replaces
  // them with something fixed.
  sanitizeIncomingIDEMessage(params: any): any {
    const params2 = JSON.parse(JSON.stringify(params));

    // Legacy IDE sends back an array of objects where those objects have
    // a '.flowVersion' field
    // LSP sends back document URLs, to files within the test project
    const url = this.getDirUrl();
    function replace(obj: Object) {
      for (const k in obj) {
        if (!obj.hasOwnProperty(k)) {
          continue;
        }
        if (obj[k] == null) {
          continue;
        }
        switch (typeof obj[k]) {
          case 'object':
            replace(obj[k]);
            break;
          case 'string':
            if (k == 'flowVersion') {
              obj[k] = '<VERSION STUBBED FOR TEST>';
            } else if (obj[k].startsWith(url)) {
              obj[k] = '<PLACEHOLDER_PROJECT_URL>' + obj[k].substr(url.length);
            }
            break;
        }
      }
    }

    replace(params2);
    return params2;
  }

  // sanitizeOutoingIDEMessage: replaces some placeholders with values
  // that can only be computed by the builder instance
  sanitizeOutgoingIDEMessage(params: Array<mixed>): Array<mixed> {
    const params2: any = JSON.parse(JSON.stringify(params));

    const dir = this.dir;
    const dirUrl = this.getDirUrl();
    function replace(obj: Object) {
      for (const k in obj) {
        if (!obj.hasOwnProperty(k)) {
          continue;
        }
        switch (typeof obj[k]) {
          case 'object':
            if (obj[k] != null) {
              replace(obj[k]);
            }
            break;
          case 'string':
            if (obj[k].startsWith('<PLACEHOLDER')) {
              obj[k] = obj[k]
                .replace(/^<PLACEHOLDER_PROJECT_DIR>/, dir)
                .replace(/^<PLACEHOLDER_PROJECT_URL>/, dirUrl);
            }
            break;
        }
      }
    }

    replace(params2);
    return params2;
  }

  async sendIDENotification(
    method: string,
    argsRaw: Array<mixed>,
  ): Promise<void> {
    const ide = this.ide;
    if (ide == null) {
      throw new Error('No IDE process running! Cannot sendIDENotification');
    }
    const args = this.sanitizeOutgoingIDEMessage(argsRaw);
    await this.log('IDE >>notification %s\n%s', method, JSON.stringify(args));
    ide.connection.sendNotification(method, ...args);
  }

  async sendIDEResponse(
    id: number | 'mostRecent',
    argsRaw: Array<mixed>,
  ): Promise<void> {
    const ide = this.ide;
    if (ide == null) {
      throw new Error('No IDE process running! Cannot sendIDEResponse');
    }
    if (id === 'mostRecent') {
      id = ide.outstandingRequestsInfo.mostRecent || 0;
    }
    const callbacks = ide.outstandingRequestsFromServer.get(id);
    if (callbacks == null) {
      throw new Error(`No request id ${id} has arrived`);
    }
    ide.outstandingRequestsFromServer.delete(id);
    if (argsRaw.length == 1 && argsRaw[0] instanceof Error) {
      const e = (argsRaw[0]: Error);
      await this.log('IDE >>response "id":%d\n%s', id, JSON.stringify(e));
      callbacks.reject(e);
    } else {
      const args = this.sanitizeOutgoingIDEMessage(argsRaw);
      await this.log('IDE >>response "id":%d\n%s', id, JSON.stringify(args));
      callbacks.resolve(...args);
    }
  }

  // This sends an IDE request and, when the response comes back, adds
  // the response to the message queue. It doesn't fulfil its returned promise
  // until that time.
  async sendIDERequestAndWaitForResponse(
    method: string,
    argsRaw: Array<mixed>,
  ): Promise<void> {
    const ide = this.ide;
    const ideMessages = this.ideMessages;
    if (ide == null) {
      throw new Error(
        'No ide process running! Cannot sendIDERequestAndWaitForResponse',
      );
    }

    const args = this.sanitizeOutgoingIDEMessage(argsRaw);
    await this.log('IDE >>request %s\n%s', method, JSON.stringify(args));
    let resultRaw;
    try {
      resultRaw = await ide.connection.sendRequest(method, ...args);
    } catch (error) {
      const message = error.message;
      error = {message, ...error}; // otherwise it doesn't show up in JSON.stringify
      ideMessages.push({method, error});
      await this.log('IDE <<error %s\n%s', method, JSON.stringify(error));
      ide.messageEmitter.emit('message');
      return;
    }
    const result = this.sanitizeIncomingIDEMessage(resultRaw);
    ideMessages.push({method, result});
    await this.log('IDE <<response %s\n%s', method, JSON.stringify(resultRaw));
    ide.messageEmitter.emit('message');
  }

  waitUntilIDEMessage(timeoutMs: number, expected: string): Promise<void> {
    const ide = this.ide;
    const ideMessages = this.ideMessages;

    return new Promise(resolve => {
      var timeout = null;
      var emitter = null;
      var alreadyDone = false;
      const startTime = new Date().getTime();

      if (ide == null) {
        this.log('No IDE process running! Cannot waitUntilIDEMessage');
        resolve();
        return;
      }

      const doneWithVerb = async verb => {
        if (alreadyDone) {
          return;
        }
        alreadyDone = true;
        const duration = new Date().getTime() - startTime;
        emitter && emitter.removeListener('message', checkLastMessage);
        timeout && clearTimeout(timeout);
        await this.log('%s message %s in %dms', verb, expected, duration);
        resolve();
      };

      const checkMessage = message => {
        if (Builder.doesMessageMatch(message, expected)) {
          doneWithVerb('Got');
        }
      };
      const checkLastMessage = () => {
        checkMessage(ideMessages.slice(-1)[0]);
      };

      // It's unavoidably racey whether the log message gets printed
      // before or after we get the right message
      this.log('Starting to wait %dms for %s', timeoutMs, expected);

      // Our backlog of messages gets cleared out at the start of each step.
      // If we've already received some messages since the start of the step,
      // let's account for them
      ideMessages.forEach(checkMessage);

      // And account for all further messages that arrive
      emitter = ide.messageEmitter.on('message', checkLastMessage);

      // ... until our stopping condition
      timeout = setTimeout(() => doneWithVerb('Failed to get'), timeoutMs);
    });
  }

  // waitUntilIDEMessageCount is a confusing method. It looks at all messages
  // that have arrived in this step so far, plus all messages that arrive
  // until the timeout, including both notifications and responses. It stops
  // as soon as either the specified number of messages or until the timeout
  // happens, whichever comes first.
  waitUntilIDEMessageCount(
    timeoutMs: number,
    expectedCount: number,
  ): Promise<void> {
    const ide = this.ide;
    const ideMessages = this.ideMessages;

    return new Promise(resolve => {
      if (ide == null) {
        this.log('No IDE process running! Cannot waitUntilIDEMessageCount');
        resolve();
        return;
      }
      if (expectedCount === 0) {
        resolve();
        return;
      }

      const onMessage = () => {
        expectedCount--;
        if (expectedCount === 0) {
          done('Received');
        }
      };
      var timeout = null;
      const done = ok => {
        this.ide &&
          this.ide.messageEmitter.removeListener('message', onMessage);
        timeout && clearTimeout(timeout);
        this.log(
          '%s all %d messages in under %dms',
          ok,
          expectedCount,
          timeoutMs,
        ).then(resolve);
      };

      // Our backlog of messages gets cleared out at the start of each step.
      // If we've already received some messages since the start of the step,
      // let's account for them:
      ideMessages.forEach(onMessage);

      // And account for all further messages that arrive, until our stopping
      // condition:
      this.log('Starting to wait %dms for messages', timeoutMs).then(() => {
        const onTimeout = () => {
          this.log('%dms timeout fired', timeoutMs).then(() =>
            done('Failed to receive'),
          );
        };
        timeout = setTimeout(onTimeout, timeoutMs);
      });

      ide.messageEmitter.on('message', onMessage);
    });
  }

  getIDEMessagesSinceStartOfStep(): Array<IDEMessage> {
    return this.ideMessages;
  }

  getIDEStderrSinceStartOfStep(): string {
    return this.ide ? this.ide.stderr.join('') : '';
  }

  clearIDEMessages(): void {
    this.ideMessages.splice(0, this.ideMessages.length);
  }

  clearIDEStderr(): void {
    this.ide && this.ide.stderr.splice(0, this.ide.stderr.length);
  }

  waitUntilIDEStatus(
    timeoutMs: number,
    expected: 'stopped' | 'running',
  ): Promise<void> {
    return new Promise(resolve => {
      var timeout = null;
      var emitter = null;
      var alreadyDone = false;
      const startTime = new Date().getTime();

      const doneWithVerb = async verb => {
        if (alreadyDone) {
          return;
        }
        alreadyDone = true;
        const duration = new Date().getTime() - startTime;
        timeout && clearTimeout(timeout);
        emitter && emitter.removeListener('ide', checkStatus);
        await this.log('%s IDE %s status in %dms', verb, expected, duration);
        resolve();
      };

      const checkStatus = () => {
        if (
          (expected === 'running' && this.ide != null) ||
          (expected === 'stopped' && this.ide == null)
        ) {
          doneWithVerb('Got');
        }
      };

      // It's unavoidably racey whether the async logger does its work before
      // or after we get the first successfull checkStatus
      this.log('Waiting up to %dms for %s IDE status', timeoutMs, expected);

      // Test whether we're okay already?
      checkStatus();

      // And look for further changes
      emitter = this.ideEmitter.on('ide', checkStatus);

      // ... until our stopping condition
      timeout = setTimeout(() => doneWithVerb('Failed to get'), timeoutMs);
    });
  }

  waitUntilServerStatus(
    timeoutMs: number,
    expected: 'stopped' | 'running',
  ): Promise<void> {
    // TODO(ljw): this should check for externally-launched flow servers too
    return new Promise(resolve => {
      var timeout = null;
      var emitter = null;
      var alreadyDone = false;
      const startTime = new Date().getTime();

      const doneWithVerb = async verb => {
        if (alreadyDone) {
          return;
        }
        alreadyDone = true;
        const duration = new Date().getTime() - startTime;
        emitter && emitter.removeListener('server', checkStatus);
        timeout && clearTimeout(timeout);
        await this.log('%s server %s status in %dms', verb, expected, duration);
        resolve();
      };

      const checkStatus = () => {
        if (
          (expected === 'running' && this.server != null) ||
          (expected === 'stopped' && this.server == null)
        ) {
          doneWithVerb('Got');
        }
      };

      // It's unavoidably racey whether the async logger does its work before
      // or after we get the first successfull checkStatus
      this.log('Waiting up to %dms for %s server status', timeoutMs, expected);

      // Test whether we're okay already?
      checkStatus();

      // And look for further changes
      emitter = this.serverEmitter.on('server', checkStatus);

      // ... until our stopping condition
      timeout = setTimeout(() => doneWithVerb('Failed to get'), timeoutMs);
    });
  }

  async cleanup(): Promise<void> {
    this.cleanupIDEConnection();
    this.stopFlowServer();
    this.closeLog();
    // We'll also do a belt-and-braces "flow stop" in case
    // any flow servers were spawned by the test that we're
    // not tracking and handling in stopFlowServer.
    // This call comes last, so that places like process.on('exit')
    // can invoke cleanup(), and at least do all the work they
    // can, even though they don't hang around waiting for flow stop
    // to finish.
    await this.flowCmd(['stop']);
  }

  assertNoErrors(): void {
    if (this.testErrors.length > 0) {
      throw new Error(
        format(
          '%d test error%s: %s',
          this.testErrors.length,
          this.testErrors.length == 1 ? '' : 's',
          this.testErrors.join('\n\n'),
        ),
      );
    }
  }

  setAllowFlowServerToDie(allow: boolean): void {
    this.allowFlowServerToDie = allow;
  }

  async forceRecheck(files: Array<string>): Promise<void> {
    if (this.server && (await isRunning(this.server.pid))) {
      const files_str = files.map(s => `"${s}"`).join(' ');
      await this.log('Running force-recheck for files: %s', files_str);
      const [err, stdout_, stderr_] = await execManual(
        format(
          '%s force-recheck --no-auto-start --temp-dir %s %s',
          this.bin,
          this.tmpDir,
          files_str,
        ),
      );
      const stderr = typeof stderr_ === 'string' ? stderr_ : stderr_.toString();
      const stdout = typeof stdout_ === 'string' ? stdout_ : stdout_.toString();
      await this.log('#force-recheck stderr...\n' + stderr);

      // No server running (6) is ok - the file change might have killed the
      // server and we raced it here
      if (err && err.code !== 6) {
        throw new Error(
          format('flow force-recheck failed!', err, stdout, stderr, files),
        );
      }
    }
  }
}

export default class Builder {
  runID: string;
  dir: string;
  errorCheckCommand: CheckCommand;

  static builders: Array<TestBuilder> = [];

  static getDirForRun(runID: string): string {
    return join(tmpdir(), 'flow/tests', runID);
  }

  // doesMethodMatch(actual, 'M') judges whether the method name of the actual
  // message was M. And doesMethodMatch(actual, 'M{C1,C2,...}') judges also
  // whether the strings C1, C2, ... were all found in the JSON representation
  // of the actual message.
  static doesMessageMatch(actual: IDEMessage, expected: string): boolean {
    const iOpenBrace = expected.indexOf('{');
    const iCloseBrace = expected.lastIndexOf('}');
    if (iOpenBrace == -1 || iCloseBrace == -1) {
      return actual.method === expected;
    } else {
      if (actual.method !== expected.substring(0, iOpenBrace)) {
        return false;
      }
      const expectedContents = expected
        .substring(iOpenBrace + 1, iCloseBrace)
        .split(',');
      const json = JSON.stringify(actual);
      for (const expectedContent of expectedContents) {
        if (!json.includes(expectedContent)) {
          return false;
        }
      }
      return true;
    }
  }

  constructor(errorCheckCommand: CheckCommand) {
    this.errorCheckCommand = errorCheckCommand;
    this.runID = randomBytes(5).toString('hex');
    this.dir = Builder.getDirForRun(this.runID);
    process.stderr.write(format('Tests will be built in %s\n', this.dir));

    // If something weird happens, lets make sure to stop all the flow servers
    // we started. Note that cleanup is an async method that does synchronous
    // cleanup and then kicks off a final 'flow stop' and awaits until it's
    // done. In our exit handler we won't live to see the end of that await.
    process.on('exit', this.cleanup);
  }

  cleanup = async () => {
    await Promise.all(Builder.builders.map(builder => builder.cleanup()));
  };

  baseDirForSuite(suiteName: string): string {
    return join(this.dir, suiteName.replace('/', 'zS'));
  }

  async createFreshTest(
    bin: string,
    suiteName: string,
    testNum: number,
    flowConfigFilename: string,
    lazyMode: 'fs' | 'ide' | null,
  ): Promise<TestBuilder> {
    const testBuilder = new TestBuilder(
      bin,
      this.errorCheckCommand,
      this.baseDirForSuite(suiteName),
      suiteName,
      testNum,
      flowConfigFilename,
      lazyMode,
    );
    Builder.builders.push(testBuilder);
    await testBuilder.createFreshDir();
    return testBuilder;
  }

  async saveResults(suiteName: string, results: SuiteResult): Promise<void> {
    const dir = this.baseDirForSuite(suiteName);
    const resultsFile = join(dir, 'results.json');
    await mkdirp(dir);
    await writeFile(
      resultsFile,
      JSON.stringify(
        {
          suiteName: suiteName,
          results,
        },
        null,
        2,
      ),
    );
  }
}
