/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {basename} = require('path');
const {format} = require('util');

const {RUNTEST_ERROR} = require('./checkRunOneTest');

import type {TestResult} from './checkRunOneTest';

export type QueueOpts = {
  parallelism: number,
  quiet: boolean,
};

type TestJob = {
  index: number,
  testDir: string,
  run: () => Promise<TestResult>,
};

class CheckRunQueue {
  _parallelism: number;
  _quiet: boolean;
  _jobs: Array<TestJob>;
  _results: Array<TestResult | null>;
  _running: number;
  _nextJob: number;
  _completed: number;
  _resolve: ?(results: Array<TestResult>) => void;
  _isTTY: boolean;

  constructor(jobs: Array<TestJob>, opts: QueueOpts) {
    this._parallelism = opts.parallelism;
    this._quiet = opts.quiet;
    this._jobs = jobs;
    this._results = (Array(jobs.length).fill(null): Array<TestResult | null>);
    this._running = 0;
    this._nextJob = 0;
    this._completed = 0;
    this._resolve = null;
    // $FlowFixMe[prop-missing] - isTTY exists on process.stdout at runtime
    this._isTTY = Boolean(process.stdout.isTTY);
  }

  run(): Promise<Array<TestResult>> {
    return new Promise(resolve => {
      this._resolve = resolve;

      if (this._jobs.length === 0) {
        resolve([]);
        return;
      }

      // Start initial batch
      const initialBatch = Math.min(this._parallelism, this._jobs.length);
      for (let i = 0; i < initialBatch; i++) {
        this._startNext();
      }
    });
  }

  _resolveWithErrors(): void {
    const resolve = this._resolve;
    if (resolve) {
      this._resolve = null;
      const results: Array<TestResult> = [];
      for (let i = 0; i < this._jobs.length; i++) {
        const existing = this._results[i];
        if (existing != null) {
          results.push(existing);
        } else {
          const name = basename(this._jobs[i].testDir);
          results.push({status: RUNTEST_ERROR, name});
        }
      }
      resolve(results);
    }
  }

  _settleJob(job: TestJob, result: TestResult): void {
    if (this._results[job.index] == null) {
      this._results[job.index] = result;
    }
    if (this._running > 0) {
      this._running--;
    }
    if (this._completed < this._jobs.length) {
      this._completed++;
    }

    setImmediate(() => {
      try {
        this._startNext();
      } catch (err) {
        process.stderr.write(
          format('Internal queue error: %s\n', err.stack || err.message || err),
        );
        this._resolveWithErrors();
        return;
      }

      try {
        this._checkDone();
      } catch (err) {
        process.stderr.write(
          format('Internal queue error: %s\n', err.stack || err.message || err),
        );
        this._resolveWithErrors();
      }
    });
  }

  _startNext(): void {
    if (this._nextJob >= this._jobs.length) {
      return;
    }

    const job = this._jobs[this._nextJob++];
    this._running++;

    if (!this._quiet && this._isTTY) {
      const name = basename(job.testDir);
      const total = this._jobs.length;
      process.stdout.write(
        format(
          '\x1b[39;49;0m[%d/%d] RUNNING:\x1b[0m %s\x1b[K\r',
          this._completed,
          total,
          name,
        ),
      );
    }

    try {
      job
        .run()
        .then(
          result => {
            this._settleJob(job, result);
          },
          err => {
            const testName = basename(job.testDir);
            process.stderr.write(
              format(
                'Error running test %s: %s\n',
                testName,
                err.stack || err.message || err,
              ),
            );
            this._settleJob(job, {
              status: RUNTEST_ERROR,
              name: testName,
            });
          },
        )
        .catch(err => {
          // Guard against errors thrown by _settleJob (via _startNext/
          // _checkDone). Without _resolveWithErrors here, the queue
          // promise would hang forever.
          process.stderr.write(
            format(
              'Internal queue error: %s\n',
              err.stack || err.message || err,
            ),
          );
          this._resolveWithErrors();
        });
    } catch (err) {
      const testName = basename(job.testDir);
      process.stderr.write(
        format(
          'Synchronous error running test %s: %s\n',
          testName,
          err.stack || err.message || err,
        ),
      );
      this._settleJob(job, {
        status: RUNTEST_ERROR,
        name: testName,
      });
    }
  }

  _checkDone(): void {
    if (this._running === 0 && this._nextJob >= this._jobs.length) {
      // Clear the TTY status line so it doesn't bleed into result output
      if (!this._quiet && this._isTTY) {
        process.stdout.write('\x1b[K');
      }
      const resolve = this._resolve;
      if (resolve) {
        this._resolve = null;
        const results: Array<TestResult> = [];
        for (let i = 0; i < this._jobs.length; i++) {
          const existing = this._results[i];
          if (existing != null) {
            results.push(existing);
          } else {
            const name = basename(this._jobs[i].testDir);
            results.push({status: RUNTEST_ERROR, name});
          }
        }
        resolve(results);
      }
    }
  }
}

module.exports = {CheckRunQueue};
