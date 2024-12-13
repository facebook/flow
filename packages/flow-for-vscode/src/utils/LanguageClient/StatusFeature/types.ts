/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

export type MessageActionItem = {
  // A short title like 'Retry', 'Open Log' etc.
  title: string;
};

// window/showStatus is a Nuclide-specific extension to LSP
// for reporting whether the LSP server is ready to handle requests
export type ShowStatusParams = {
  // The message type, {@link MessageType}. Permits only Error, Warning, Info.
  type: number;
  // The message action items to present. Only allowed for Error.
  actions?: Array<MessageActionItem>;
  // The actual message. Mandatory for Error, Warning.
  message?: string;
  // An optional short message, hopefully <8chars, to appear prominently
  shortMessage?: string;
  // The client might display a progress bar "numerator/denominator" if both are
  // present, or an indeterminate progress bar if only numerator is present.
  progress?: { numerator: number; denominator?: number };
};

export const LspMessageType = {
  // An error message.
  Error: 1,
  // A warning message.
  Warning: 2,
  // An information message.
  Info: 3,
  // A log message.
  Log: 4,
};

export type StatusData =
  | { kind: 'null' }
  | { kind: 'green'; message?: string }
  | {
      kind: 'yellow';
      id?: string;
      message: string;
      buttons: Array<string>;
      shortMessage?: string;
      progress?: { numerator: number; denominator?: number };
    }
  | {
      kind: 'red';
      id?: string;
      message: string;
      buttons: Array<string>;
    };

export type ActionItem = {
  title: string;
  command: () => void;
};

export type StatusReport =
  | {
      state: 'busy';
      progress: string;
      message: string;
      actions?: Array<ActionItem>;
    }
  | { state: 'idle'; message: string; actions?: Array<ActionItem> }
  | { state: 'error'; message: string; actions?: Array<ActionItem> };

export type StatusOptions = {
  onChange: (status: StatusReport | null) => void;
};
