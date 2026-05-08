/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

// https://github.com/flow-typed/flow-typed/blob/4cce7357490b1bcf3f5dc33a9599017f080fb1d6/definitions/npm/%40babel/code-frame_v7.x.x/flow_v0.104.x-/code-frame_v7.x.x.js

declare module '@babel/code-frame' {
  declare export interface SourceLocation {
    +start: interface {
      +line: number,
      +column: number,
    };
    +end?: interface {
      +line: number,
      +column: number,
    };
  }
  declare export function codeFrameColumns(
    rawLines: string,
    location: SourceLocation,
    options?: BabelCodeFrameOptions,
  ): string;

  declare export type BabelCodeFrameOptions = {
    /**
     * Syntax highlight the code as JavaScript for terminals. default: false
     */
    highlightCode?: boolean,

    /**
     * The number of lines to show above the error. default: 2
     */
    linesAbove?: number,

    /**
     * The number of lines to show below the error. default: 3
     */
    linesBelow?: number,

    /**
     * Forcibly syntax highlight the code as JavaScript (for non-terminals);
     * overrides highlightCode.
     * default: false
     */
    forceColor?: boolean,

    /**
     * Pass in a string to be displayed inline (if possible) next to the
     * highlighted location in the code. If it can't be positioned inline,
     * it will be placed above the code frame.
     * default: nothing
     */
    message?: string,
    ...
  };

  /**
   * Generate errors that contain a code frame that point to source locations.
   * @param rawLines Raw lines to frame
   * @param lineNumber Line number (1 indexed)
   * @param colNumber Column number
   * @param options Additional options
   * @returns Framed code
   */
  declare export default function codeFrame(
    rawLines: string,
    lineNumber: number,
    colNumber: number,
    options?: BabelCodeFrameOptions,
  ): string;
}
