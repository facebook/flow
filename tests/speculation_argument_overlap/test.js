// @flow

declare function overload(key: boolean): string;
declare function overload(key: boolean): number;

() => {
  const a = overload(key);
}

declare var key: boolean;
