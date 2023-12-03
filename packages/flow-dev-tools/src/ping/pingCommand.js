/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {Flag} from '../command/Base';

const child_process = require('child_process');
const rpc = require('vscode-jsonrpc');
const uri = require('vscode-uri');

const path = require('path');

const {commonFlags, default: Base} = require('../command/Base');
const findFlowBin = require('../command/findFlowBin').default;

type Args = {
  bin: string,
  root: string,
};

const PingRequest = new rpc.RequestType('telemetry/ping');

class PingCommand extends Base<Args> {
  static processArgv(argv: any): Args {
    return {
      bin: findFlowBin(argv.bin),
      root: path.resolve(process.cwd(), argv.root),
    };
  }

  static async run(args: Args): Promise<void> {
    const rootUri = uri.URI.file(args.root).toString();

    const lsp = child_process.spawn(args.bin, ['lsp', '--autostop']);
    const conn = rpc.createMessageConnection(
      new rpc.StreamMessageReader(lsp.stdout),
      new rpc.StreamMessageWriter(lsp.stdin),
    );
    conn.listen();

    const initializeResponse = await conn.sendRequest('initialize', {
      rootUri: rootUri,
      capabilities: {},
    });

    async function doPing(): Promise<number> {
      const start = Date.now();
      const response = await conn.sendRequest(PingRequest);
      const duration = Date.now() - start;
      return {start, duration, ...response};
    }

    async function throttle(ms: number): Promise<void> {
      await new Promise(resolve => setTimeout(resolve, ms));
    }

    while (true) {
      const [pong, _] = await Promise.all([doPing(), throttle(100)]);
      console.log(JSON.stringify(pong));
    }
  }

  static description(): string {
    return 'Measures IDE command latency';
  }

  static async usage(): Promise<string> {
    return `usage: ${process.argv[1]} ping`;
  }

  static getFlags(): Array<Flag> {
    return [
      commonFlags.bin,
      {
        type: 'string',
        name: 'root',
        argName: 'ROOT',
        description: 'The Flow root to profile',
        default: '.',
      },
    ];
  }
}

module.exports = {
  default: PingCommand,
};
