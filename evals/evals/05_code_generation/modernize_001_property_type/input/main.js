/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type ServerConfig = {
  host: string,
  port: number,
  useTLS: boolean,
};

type Port = $PropertyType<ServerConfig, 'port'>;

export function formatPort(port: Port): string {
  return port === 443 ? 'https' : 'port ' + String(port);
}
