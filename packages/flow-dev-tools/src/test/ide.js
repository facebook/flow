/**
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
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

export type IDEMessage =
  | {method: string, params: $ReadOnlyArray<mixed>} // Notification
  | {result: Array<mixed>}; // Autocomplete result
