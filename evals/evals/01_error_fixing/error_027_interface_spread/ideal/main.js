/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

interface QuotaMeter {
  used: number;
  limit: number;
  consume(n: number): number;
}

type AuditEntry = {
  used: number,
  limit: number,
  caller: string,
  at: number,
};

function snapshotUsage(meter: QuotaMeter, caller: string): AuditEntry {
  return {
    used: meter.used,
    limit: meter.limit,
    caller,
    at: Date.now(),
  };
}

declare const upload: QuotaMeter;
declare const download: QuotaMeter;

upload.consume(512);
download.consume(1024);

const entries: Array<AuditEntry> = [
  snapshotUsage(upload, 'upload-worker'),
  snapshotUsage(download, 'download-worker'),
];

const remaining = entries.reduce(
  (acc, e) => acc + (e.limit - e.used),
  0,
);

console.log(`entries=${entries.length} remaining=${remaining}`);
