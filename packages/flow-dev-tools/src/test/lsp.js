/**
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
  | {method: string, params: $ReadOnlyArray<mixed>} // Notification/request
  | {method: string, result: mixed} // Response
  | {method: string, error: mixed}; // Response-error
