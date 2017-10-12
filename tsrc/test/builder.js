/* @flow */

import {execSync, spawn} from 'child_process';
import {randomBytes} from 'crypto';
import {tmpdir} from 'os';
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

type CheckCommand = 'check' | 'status';


export class TestBuilder {
  bin: string;
  dir: string;
  errorCheckCommand: CheckCommand;
  flowConfigFilename: string;
  lazyMode: 'ide' | 'fs' | null;
  server: null | child_process$ChildProcess = null;
  ide: null | {
    connection: RpcConnection;
    process: child_process$ChildProcess;
    messages: Array<IDEMessage>;
    stderr: Array<string>;
    emitter: EventEmitter;
  } = null;
  sourceDir: string;
  suiteName: string;
  tmpDir: string;
  testErrors = [];
  allowFlowServerToDie = false;

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
    this.dir = join(
      baseDir,
      String(testNum),
    );
    this.sourceDir = join(
      getTestsDir(),
      suiteName
    );
    this.tmpDir = join(
      baseDir,
      "tmp",
      String(testNum),
    );
    this.flowConfigFilename = flowConfigFilename;
    this.lazyMode = lazyMode;
  }

  getFileName(): string {
    return join(this.dir, 'test.js');
  }

  normalizeForFlowconfig(path: string): string {
    return path.split(dir_sep).join('/');
  }

  async createFreshDir(): Promise<void> {
    await mkdirp(this.dir);
    await mkdirp(this.tmpDir);

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
      let config = configBuffer.toString().split("\n");
      let temp_dir = null;
      let options_index = null;
      config.forEach((line, index) => {
        const match = line.trim().match("^temp_dir=(.*)$");
        match != null && (temp_dir = match[1])
        line.trim() == "[options]" && (options_index = index);
      });

      if (temp_dir == null) {
        if (options_index == null) {
          config.push("[options]");
          options_index = config.length - 1;
        }
        config.splice(
          options_index + 1,
          0,
          "temp_dir=" + this.normalizeForFlowconfig(this.tmpDir),
        );
      } else {
        this.tmpDir = temp_dir;
      }
      await writeFile(join(this.dir, ".flowconfig"), config.join("\n"));
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
    await writeFile(this.getFileName(), "/* @flow */\n");
  }

  async addCode(code: string): Promise<void> {
    const filename = this.getFileName();
    await appendFile(filename, "\n"+code+"\n");
    await this.forceRecheck([filename]);
  }

  async addFileImpl(source: string, dest: string): Promise<string> {
    source = join(this.sourceDir, source);
    dest = join(this.dir, dest);
    const contents_buffer = await readFile(source);
    let contents = contents_buffer.toString();
    if (contents.match(/@thisWillBeFlowInTest/)) {
      // Undo what the convert command did
      contents = contents.replace(/@thisWillBeFlowInTest/, "@flow");
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
      sources.map(source => this.addFileImpl(source, source))
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
      files.map(file => this.removeFileImpl(file))
    );
    await this.forceRecheck(filenames);
  }

  async flowCmd(
    args: Array<string>,
    stdinFile?: string,
  ): Promise<[number, string, string]> {
    let cmd = format(
      "%s %s %s",
      this.bin,
      args.map(arg => format('"%s"', arg)).join(" "),
      stdinFile == null ? "" : format("< %s", stdinFile),
    );
    const [err, stdout, stderr] = await execManual(cmd, {cwd: this.dir});
    const code = err == null ? 0 : err.code;

    return [code, stdout.toString(), stderr.toString()];
  }

  async getFlowErrors(retry?: bool = true): Promise<Object> {
    let cmd;
    if (this.errorCheckCommand === 'check') {
      cmd = format(
        "%s check --strip-root --temp-dir %s --json %s",
        this.bin,
        this.tmpDir,
        this.dir,
      );
    } else {
      // No-op if it's already running
      await this.startFlowServer();
      cmd = format(
        "%s status --no-auto-start --strip-root --temp-dir %s --json %s",
        this.bin,
        this.tmpDir,
        this.dir,
      );
    }

    const [err, stdout, stderr] = await execManual(
      cmd,
      {cwd: __dirname, maxBuffer: 1024 * 1024},
    );

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
    const lazyMode = this.lazyMode === null
      ? []
      : ['--lazy-mode', this.lazyMode];
    const serverProcess = spawn(
      this.bin,
      [
        'server',
        '--strip-root',
        '--debug',
        '--temp-dir', this.tmpDir,
      ].concat(lazyMode)
      .concat([
        this.dir,
      ]),
      {
        // Useful for debugging flow server
        // stdio: ["pipe", "pipe", process.stderr],
        cwd: this.dir,
        env: { ...process.env, 'OCAMLRUNPARAM': 'b' },
      }
    );
    this.server = serverProcess;

    const stderr = [];
    serverProcess.stderr.on('data', (data) => {
      stderr.push(data.toString());
    });

    serverProcess.on('exit', (code, signal) => {
      if (this.server != null && !this.allowFlowServerToDie) {
        this.testErrors.push(format(
          "flow server mysteriously died. Code: %d, Signal: %s, stderr:\n%s",
          code,
          signal,
          stderr.join(""),
        ));
      }
      this.stopFlowServer();
    });

    // Wait for the server to be ready
    await new Promise((resolve, reject) => {
      function resolveOnReady(data) {
        if (stderr.concat([data]).join('').match(/Server is READY/)) {
          serverProcess.stderr.removeListener('data', resolveOnReady);
          resolve();
        }
      }
      serverProcess.stderr.on('data', resolveOnReady);
    });
  }

  stopFlowServer(): void {
    const server = this.server;
    if (server != null) {
      this.server = null;
      server.stdin.end();
      server.kill();
    }
  }

  async createIDEConnection(): Promise<void> {
    if (this.ide == null) {
      // No-op if the server is already running
      await this.startFlowServer();
      const ideProcess = spawn(
        this.bin,
        [
          'ide',
          '--protocol', 'very-unstable',
          '--no-auto-start',
          '--strip-root',
          '--temp-dir', this.tmpDir,
          '--root', this.dir,
        ],
        {
          // Useful for debugging flow ide
          // stdio: ["pipe", "pipe", process.stderr],
          cwd: this.dir,
          env: { ...process.env, 'OCAMLRUNPARAM': 'b' },
        }
      );
      const connection = rpc.createMessageConnection(
        new rpc.StreamMessageReader(ideProcess.stdout),
        new rpc.StreamMessageWriter(ideProcess.stdin),
      );
      connection.listen();

      ideProcess.on('exit', (code, signal) => {
        if (this.ide != null) {
          this.testErrors.push(format(
            "flow ide mysteriously died. Code: %d, Signal: %s, stderr:\n%s",
            code,
            signal,
            this.getIDEStderr(),
          ));
        }
        this.cleanupIDEConnection();
      });
      ideProcess.on('close', () => this.cleanupIDEConnection());

      const emitter = new EventEmitter;

      const messages = [];
      connection.onNotification((method: string, ...params: Array<mixed>) => {
        params.forEach(param => {
          if (typeof param === "object" && param && param.flowVersion) {
            param.flowVersion = "<VERSION STUBBED FOR TEST>";
          }
        });
        messages.push({method, params});
        emitter.emit('notification', {method, params});
      });

      const stderr = [];
      ideProcess.stderr.on('data', (data) => {
        stderr.push(data.toString());
      });

      this.ide = { process: ideProcess, connection, messages, stderr, emitter };
    }
  }

  cleanupIDEConnection(): void {
    const ide = this.ide;
    if (ide != null) {
      this.ide = null;
      ide.process.stdin.end();
      ide.process.kill();
      ide.connection.dispose();
    }
  }

  sendIDENotification(methodName: string, args: Array<mixed>): void {
    if (this.ide != null) {
      this.ide.connection.sendNotification(methodName, ...args);
    }
  }

  async sendIDERequest(methodName: string, args: Array<mixed>): Promise<void> {
    const ide = this.ide;
    if (ide != null) {
      ide.messages.push(
        await ide.connection.sendRequest(methodName, ...args),
      );
    }
  }

  ideNewMessagesWithTimeout(
    timeoutMs: number,
    expected: $ReadOnlyArray<IDEMessage>,
  ): Promise<void> {
    const ide = this.ide;
    const messages = [...expected];

    return new Promise(resolve => {
      if (ide == null) {
        throw new Error('No ide process running!');
      }
      if (expected.length === 0) {
        resolve();
        return; // Flow doesn't know resolve doesn't return
      }

      const onNotification = message => {
        const next = messages.shift();
        // While we could end early if the message doesn't match, let's wait for
        // either the timeout or the same number of messages
        if (messages.length === 0) {
          done();
        }
      };
      const done = () => {
        this.ide &&
          this.ide.emitter.removeListener('notification', onNotification);
        resolve();
      }

      // If we've already received some notifications then process them.
      ide.messages.forEach(onNotification);

      setTimeout(done, timeoutMs);

      ide.emitter.on('notification', onNotification);
    });
  }

  getIDEMessages(): Array<IDEMessage> {
    return this.ide ? [...this.ide.messages] : [];
  }

  getIDEStderr(): string {
    return this.ide ? this.ide.stderr.join("") : "";
  }

  clearIDEMessages(): void {
    this.ide && (this.ide.messages.splice(0, this.ide.messages.length));
  }

  clearIDEStderr(): void {
    this.ide && (this.ide.stderr.splice(0, this.ide.stderr.length));
  }

  cleanup(): void {
    this.cleanupIDEConnection();
    this.stopFlowServer();
  }

  assertNoErrors(): void {
    if (this.testErrors.length > 0) {
      throw new Error(format(
        "%d test error%s: %s",
        this.testErrors.length,
        this.testErrors.length == 1 ? "" : "s",
        this.testErrors.join("\n\n"),
      ));
    }
  }

  async waitForServerToDie(timeout: number): Promise<void> {
    let remaining = timeout;
    while (remaining > 0 && this.server != null) {
      await sleep(Math.min(remaining, 100));
      remaining -= 100;
    }
  }

  setAllowFlowServerToDie(allow: boolean): void {
    this.allowFlowServerToDie = allow;
  }

  async forceRecheck(files: Array<string>): Promise<void> {
    if (this.server && await isRunning(this.server.pid)) {
      const [err, stdout, stderr] = await execManual(format(
        "%s force-recheck --no-auto-start --temp-dir %s %s",
        this.bin,
        this.tmpDir,
        files.map(s => `"${s}"`).join(" "),
      ));

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

  constructor(errorCheckCommand: CheckCommand) {
    this.errorCheckCommand = errorCheckCommand;
    this.runID = randomBytes(5).toString('hex');
    this.dir = Builder.getDirForRun(this.runID);
    process.stderr.write(format("Tests will be built in %s\n", this.dir));

    // If something weird happens, lets make sure to stop all the flow servers
    // we started
    process.on('exit', this.cleanup);
  }

  cleanup = () => {
    Builder.builders.forEach(builder => builder.cleanup());
  }

  baseDirForSuite(suiteName: string): string {
    return join(this.dir, suiteName.replace("/", "zS"));
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
    const resultsFile = join(dir, "results.json");
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
