/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* @flow*/
import Logger from './Logger';
import importFresh from './importFresh';
import getExtensionPath from './getExtentionPath';
import { promises, createReadStream } from 'fs';
import path from 'path';
// $FlowFixMe Flow doesn't recognize crypto.verify
import { verify, createHash } from 'crypto';
import { window } from 'vscode';

// fs/promises
const { readFile, readdir, access } = promises;

function getFlowBinDirPrefixForPlatform(): null | string {
  return process.platform === 'darwin'
    ? 'flow-osx-v'
    : process.platform === 'linux' && process.arch === 'x64'
    ? 'flow-linux64-v'
    : process.platform === 'win32' && process.arch === 'x64'
    ? 'flow-win64-v'
    : null;
}

async function getFlowBinRelativePath(
  flowBinModulePath: string,
): Promise<{ flowBinDirName: string; flowBinName: string }> {
  const dirPrefix = getFlowBinDirPrefixForPlatform();
  if (!dirPrefix) {
    throw new Error(
      `Failed to determine correct binary for platform ${process.platform} and arch ${process.arch}`,
    );
  }
  const flowBinModuleContents = await readdir(flowBinModulePath);
  const flowBinDirName = flowBinModuleContents.find((x) =>
    x.startsWith(dirPrefix),
  );
  if (!flowBinDirName) {
    throw new Error(
      `Failed to find anything starting with ${dirPrefix} in ${flowBinModulePath}`,
    );
  }
  const flowBinDir = path.join(flowBinModulePath, flowBinDirName);
  const flowBinDirContents = await readdir(flowBinDir);
  const flowBinName = flowBinDirContents.find((x) => x.startsWith('flow'));
  if (!flowBinName) {
    throw new Error(`Failed to find flow binary in ${flowBinDir}`);
  }
  return { flowBinDirName, flowBinName };
}

async function getShasums(
  flowBinModulePath: string,
  logger: Logger,
): Promise<Buffer> {
  const extensionPath = getExtensionPath();
  try {
    // try veryifying against SHASUM256.txt.sign
    const shasums = await readFile(
      path.join(flowBinModulePath, 'SHASUM256.txt'),
    );
    const shasumsSignatureBase64 = await readFile(
      path.join(flowBinModulePath, 'SHASUM256.txt.sign'),
      'ascii',
    );
    const shasumsSignature = Buffer.from(shasumsSignatureBase64, 'base64');
    const publicKey = await readFile(path.join(extensionPath, 'signing.pem'));
    if (!verify('sha256', shasums, publicKey, shasumsSignature)) {
      throw new Error('Failed to verify SHASUM256.txt against public key');
    }
    return shasums;
  } catch (err: any) {
    logger.info(`Unable to verify SHASUM256.txt.sign:\n${err.message}`);
    return readFile(path.join(extensionPath, 'PAST_FLOW_BIN_SHASUMS.txt'));
  }
}

function getShasum(
  shasums: string,
  flowBinDirName: string,
  flowBinName: string,
): string {
  const flowBinRelativePath = `${flowBinDirName}/${flowBinName}`;
  // eslint-disable-next-line require-unicode-regexp
  const shasumLines = shasums.split(/\r?\n/);
  const shasumLine = shasumLines.find((line) =>
    line.includes(flowBinRelativePath),
  );
  if (!shasumLine) {
    throw new Error(
      `Failed to find SHASUM256.txt line containing ${flowBinRelativePath}`,
    );
  }
  return shasumLine.slice(0, 64);
}

export default async function getVerifiedFlowBinPath(
  flowBinModulePath: string,
  logger: Logger,
): Promise<string> {
  await access(flowBinModulePath);
  try {
    const shasums = await getShasums(flowBinModulePath, logger);

    // successfully verified SHASUM256.txt, now we can use it to verify the flow binary
    const { flowBinDirName, flowBinName } = await getFlowBinRelativePath(
      flowBinModulePath,
    );
    const flowBinPath = path.join(
      flowBinModulePath,
      flowBinDirName,
      flowBinName,
    );
    const hash = createHash('sha256');
    const flowBinReadStream = createReadStream(flowBinPath);
    const flowBinHashPromise = new Promise((resolve, reject) => {
      flowBinReadStream.on('end', () => resolve(hash.digest('hex')));
      flowBinReadStream.on('error', reject);
    });
    flowBinReadStream.pipe(hash);
    const flowBinHash = await flowBinHashPromise;
    const shasum = getShasum(shasums.toString(), flowBinDirName, flowBinName);
    if (flowBinHash !== shasum) {
      throw new Error(
        `Hash of ${flowBinPath} does not match hash from SHASUM256.txt:\n` +
          `Hash of flow binary: ${flowBinHash}\n` +
          `Hash from SHASUM256.txt: ${shasum}`,
      );
    }
    return flowBinPath;
  } catch (err: any) {
    logger.error(
      `Error when verifying flow-bin in ${flowBinModulePath}:\n${err.message}`,
    );
    // failed to verify SHASUM256.txt; ask the user whether to proceed anyway
    const quickPickOptions = {
      title: `Unable to verify the integrity of ${flowBinModulePath}. Proceed anyway?`,
    };
    const quickPickItems = [
      {
        label: `Don't try to use ${flowBinModulePath}`,
        proceedAnyay: false,
      },
      {
        label: `Try to use ${flowBinModulePath} anyway`,
        proceedAnyay: true,
      },
    ];

    const userSelection = await window.showQuickPick(
      quickPickItems,
      quickPickOptions,
    );

    if (userSelection && userSelection.proceedAnyay) {
      // user can change version of module or remove module while plugin is running
      // so always importFresh
      return importFresh(flowBinModulePath);
    }
    throw new Error('User chose not to use unverified flow-bin');
  }
}
