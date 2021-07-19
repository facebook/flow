/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

export type MessageHandler = (...args: any) => mixed;
export type RpcConnection = {
  onNotification(methodName: string, handler: MessageHandler): void,
  sendNotification(methodName: string, ...args: any): void,
  sendRequest(methodName: string, ...args: any): Promise<any>,
  // TODO requests
  listen(): void,
  dispose(): void,
};

export type LSPMessage =
  | {method: string, params: $ReadOnlyArray<mixed> | {...} | void} // Notification/request
  | {method: string, result: $ReadOnlyArray<mixed> | {...} | null} // Response
  | {method: string, error: mixed}; // Response-error
