// @flow

declare module 'declared-module' {
  declare enum G { // ERROR: enums not enabled
    J,
    K,
  }
  declare export enum H { // ERROR: enums not enabled
    X,
    Y,
  }
}
