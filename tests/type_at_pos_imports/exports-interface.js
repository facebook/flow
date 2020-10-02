// @flow

export interface Logger {
  isActive(): boolean;
  start(): void;
  stop(): void;
}

export interface Listener<TFields> {
  isEnabled(): boolean;
  enable(): void;
  disable(): void;
  toggle(): void;
  listen(eventCategory: ?string, eventName: string, data: TFields): void;
}

export default 1;
