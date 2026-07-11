// @flow

declare function track(event: string, payload: {userId: string}): void;

declare namespace Analytics {
  declare const appId: string;
}
