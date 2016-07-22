/* @flow */

import {execSync} from 'child_process';
import {randomBytes} from 'crypto';
import {tmpdir} from 'os';
import {basename, dirname, extname, join, sep as dir_sep} from 'path';
import {format} from 'util';

import {appendFile, exec, execManual, mkdirp, readdir, readFile, writeFile} from '../async';
import {testsDir} from '../constants';

import type {SuiteResult} from './runTestSuite';

type CheckCommand = 'check' | 'status';

export class TestBuilder {
  bin: string;
  dir: string;
  errorCheckCommand: CheckCommand;
  flowConfigFilename: string;
  server: ?number;
  sourceDir: string;
  suiteName: string;
  tmpDir: string;

  constructor(
    bin: string,
    errorCheckCommand: CheckCommand,
    baseDir: string,
    suiteName: string,
    testNum: number,
    flowConfigFilename: string,
  ) {
    this.bin = bin;
    this.errorCheckCommand = errorCheckCommand;
    this.suiteName = suiteName;
    this.dir = join(
      baseDir,
      String(testNum),
    );
    this.sourceDir = join(
      testsDir,
      suiteName
    );
    this.tmpDir = join(
      baseDir,
      "tmp",
      String(testNum),
    );
    this.flowConfigFilename = flowConfigFilename;
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
          "%s init --options 'all=true;temp_dir=%s' %s",
          this.bin,
          this.tmpDir,
          this.dir,
        ),
        {cwd: __dirname},
      );
    }
    await writeFile(this.getFileName(), "/* @flow */\n");
  }

  async addCode(code: string): Promise<void> {
    await appendFile(this.getFileName(), "\n"+code+"\n");
  }

  async addFile(source: string, dest: string): Promise<void> {
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
  }

  async addFiles(sources: Array<string>): Promise<void> {
    await Promise.all(
      sources.map(source => this.addFile(source, source))
    );
  }

  async flowCmd(args: Array<string>, stdinFile?: string): Promise<[number, string, string]> {
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
    const [err, stdout, stderr] = await execManual(format(
      "%s start --json --strip-root --temp-dir %s --wait %s",
      this.bin,
      this.tmpDir,
      this.dir,
    ));

    if (!err) {
      const response = JSON.parse(stdout.toString());
      this.server = response.pid;
    } else if (err.code === 11) {
      // Already a server running...that's cool
    } else {
      throw new Error(format('Flow start failed!', err, stdout, stderr));
    }
  }

  async stopFlowServer(): Promise<void> {
    const server = this.server;
    if (server != null ) {
      this.server = null;
      const [err, stdout, stderr] = await execManual(format(
        "%s stop %s",
        this.bin,
        this.dir,
      ));

      if (err != null) {
        try {
          process.kill(server);
        } catch (e) {
          console.log("Failed to kill server %s", server);
        }
      }
    }
  }

  stopFlowServerSync(): void {
    const server = this.server;
    if (server != null) {
      this.server = null;
      try {
        execSync(format("%s stop %s", this.bin, this.dir));
      } catch (e) {
        try {
          process.kill(server);
        } catch (e) {
          console.log("Failed to kill server %s", server);
        }
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
    process.on('exit', () => {
      Builder.builders.forEach(builder => builder.stopFlowServerSync());
    });
  }

  baseDirForSuite(suiteName: string): string {
    return join(this.dir, suiteName.replace("/", "zS"));
  }

  async createFreshTest(
    bin: string,
    suiteName: string,
    testNum: number,
    flowConfigFilename: string,
  ): Promise<TestBuilder> {
    const testBuilder = new TestBuilder(
      bin,
      this.errorCheckCommand,
      this.baseDirForSuite(suiteName),
      suiteName,
      testNum,
      flowConfigFilename,
    );
    Builder.builders.push(testBuilder);
    await testBuilder.createFreshDir(suiteName);
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
