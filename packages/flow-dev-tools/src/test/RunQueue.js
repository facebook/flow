/**
 * @flow
 * @format
 */

import colors from 'colors/safe';
import {format} from 'util';
import tty from 'tty';

import runTestSuite from './runTestSuite';

import type Builder from './builder';
import type Suite from './Suite';
import type {SuiteResult} from './runTestSuite';

/* Cool little thingy that runs tests in parallel and prints out a multiline
 * status view of what is running and what is done */
export default class RunQueue {
  almostDone: Array<string>;
  bin: string;
  builder: Builder;
  done: Array<string>;
  fancyStatusSize: number;
  fbmakeJsonResults: Array<{
    name: string,
    status: 'passed' | 'failed',
    start_time: number,
    end_time: number,
    details: string,
  }>;
  isFbmakeJson: boolean;
  isTTY: boolean;
  maxLength: number;
  onDone: () => void;
  parallelism: number;
  results: {[key: string]: SuiteResult};
  writeResults: Array<Promise<void>> = [];
  running: Array<string>;
  statuses: {[key: string]: {status: string, details: string}};
  suites: {[key: string]: Suite};
  todo: Array<string>;

  constructor(
    bin: string,
    parallelism: number,
    isFbmakeJson: boolean,
    suites: {[key: string]: Suite},
    builder: Builder,
  ) {
    this.parallelism = parallelism;
    this.isFbmakeJson = isFbmakeJson;
    this.fbmakeJsonResults = [];
    this.isTTY =
      !isFbmakeJson && !!process.stdout.isTTY && process.platform !== 'win32';
    this.bin = bin;
    this.builder = builder;
    this.results = {};
    this.statuses = {};
    this.suites = suites;
    this.todo = Object.keys(suites).sort();
    this.running = [];
    this.almostDone = [];
    this.done = [];
    this.onDone = () => {};
    this.maxLength = Math.max(...this.todo.map(name => name.length));
    this.fancyStatusSize = 0;

    this.todo.forEach(suiteName => {
      this.statuses[suiteName] = {
        status: colors.grey.bold('[ ] RUN') + ': ',
        details: '',
      };
    });
  }

  async go(): Promise<void> {
    process.stderr.write(format('Running %d suites\n', this.todo.length));
    this.isTTY && process.stdout.write('\x1B[?25l'); // Hide terminal cursor
    const donePromise = new Promise((resolve, reject) => {
      this.onDone = resolve;
    });
    if (this.todo.length > 0) {
      if (this.parallelism < 1) {
        throw new Error(
          'Parallelism must be >=1, given ' + String(this.parallelism),
        );
      }
      this.todo.slice(0, this.parallelism).forEach(() => this.start()),
        await donePromise;
    } else {
      this.checkDone();
    }
    this.isTTY && process.stdout.write('\x1B[?25h'); // Show terminal cursor
    process.stdout.write('Flushing results to disk...\n');
    await Promise.all(this.writeResults);
  }

  async start(): Promise<void> {
    const suiteName = this.todo.shift();
    if (suiteName == undefined) {
      throw new Error('OH NOES');
    }
    this.running.push(suiteName);
    var start_time = new Date().getTime() / 1000;
    if (this.isFbmakeJson) {
      process.stderr.write(
        JSON.stringify({
          op: 'start',
          test: suiteName,
        }),
      );
      process.stderr.write('\n');
    }
    const suiteResult = await runTestSuite(
      this.bin,
      this.builder,
      suiteName,
      this.suites[suiteName],
      this.reportStatus.bind(this, suiteName),
    );
    var end_time = new Date().getTime() / 1000;
    this.results[suiteName] = suiteResult;
    // No need to await these immediately, but we will make sure all writes
    // finish before the RunQueue exits
    this.writeResults.push(this.builder.saveResults(suiteName, suiteResult));
    if (this.isFbmakeJson) {
      let details = '';
      let status = 'passed';
      if (suiteResult.type === 'exceptional') {
        status = 'failed';
        details += format('ERROR: suite %s\n%s', suiteResult.message);
      } else if (suiteResult.type === 'normal') {
        const testResults = suiteResult.testResults;
        for (let testNum = 0; testNum < testResults.length; testNum++) {
          const testResult = testResults[testNum];
          for (
            let stepNum = 0;
            stepNum < testResult.stepResults.length;
            stepNum++
          ) {
            const result = testResult.stepResults[stepNum];
            if (!result.passed) {
              status = 'failed';
              details += format(
                'FAILED: suite %s, test %s (%d of %d), step %d of %d\n',
                suiteName,
                testResult.name || 'unnamed test',
                testNum + 1,
                testResults.length,
                stepNum + 1,
                testResult.stepResults.length,
              );
              const messages = [];
              for (const assertionResult of result.assertionResults) {
                if (assertionResult.type === 'fail') {
                  messages.push(...assertionResult.messages);
                }
              }
              details += messages.join('\n');
            }
          }
        }
      }
      process.stderr.write(
        JSON.stringify({
          op: 'test_done',
          test: suiteName,
          status,
          start_time,
          end_time,
          details,
        }),
      );
      process.stderr.write('\n');

      this.fbmakeJsonResults.push({
        name: suiteName,
        status,
        start_time,
        end_time,
        details,
      });
    }

    this.almostDone.push(suiteName);
    this.running = this.running.filter(name => name != suiteName);
    this.processAlmostDone();
    this.printFancyStatus();

    this.checkDone();
  }

  processAlmostDone() {
    if (this.almostDone.length > 0) {
      this.almostDone = this.almostDone.sort();
      while (
        this.almostDone.length > 0 &&
        (this.running.length == 0 || this.almostDone[0] <= this.running[0])
      ) {
        const suiteName = this.almostDone.shift();
        this.done.push(suiteName);
        const stdout = process.stdout;
        if (this.isTTY && stdout instanceof tty.WriteStream) {
          stdout.moveCursor(0, -this.fancyStatusSize);
          stdout.cursorTo(0);
          stdout.clearLine(0);
          this.printSuiteStatus(suiteName);
          this.fancyStatusSize > 0 && this.fancyStatusSize--;
          stdout.moveCursor(0, this.fancyStatusSize);
        } else if (!this.isFbmakeJson) {
          this.printSuiteStatus(suiteName);
        }
      }
    }
  }

  reportStatus(suiteName: string, status: string, details: string) {
    this.statuses[suiteName] = {
      status,
      details,
    };
    this.printFancyStatus();
  }

  checkDone() {
    if (this.todo.length > 0) {
      this.start();
    } else if (this.running.length === 0) {
      if (this.isFbmakeJson) {
        process.stderr.write(
          JSON.stringify({
            op: 'all_done',
            results: this.fbmakeJsonResults,
          }),
        );
      }
      process.stderr.write('\n');
      this.onDone();
    }
  }

  printSuiteStatus(suiteName: string) {
    const {status, details} = this.statuses[suiteName];
    const paddingLength = this.maxLength - suiteName.length;
    const padding = paddingLength > 0 ? Array(paddingLength + 1).join(' ') : '';
    process.stdout.write(
      format(
        '%s ' + colors.blue('%s') + '%s %s\n',
        status,
        suiteName,
        padding,
        details,
      ),
    );
  }

  printFancyStatus() {
    if (!this.isTTY) {
      return;
    }
    // $FlowFixMe Add to lib
    process.stdout.clearLine();
    for (let i = 0; i < this.fancyStatusSize; i++) {
      // $FlowFixMe Add to lib
      process.stdout.moveCursor(0, -1);
      // $FlowFixMe Add to lib
      process.stdout.clearLine();
    }
    const numLines = this.running.length;
    this.fancyStatusSize = numLines;
    process.stdout.write(Array(numLines + 1).join('\n'));
    // $FlowFixMe Add to lib
    process.stdout.moveCursor(0, -numLines);
    const suites = this.running.sort();
    for (const suiteName of suites) {
      this.printSuiteStatus(suiteName);
    }
    process.stdout.write(
      format(
        'TODO: %d  |  RUNNING: %d  |  DONE: %d',
        this.todo.length,
        this.running.length,
        this.done.length + this.almostDone.length,
      ),
    );
  }
}
