/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import { type Uri } from 'vscode';
import isWindows from './isWindows';

const CharCode = {
  Colon: 58,
  Slash: 47,
  A: 65,
  Z: 90,
  a: 97,
  z: 122,
};

export default function uriToString(uri: Uri): string {
  return isWindows() ? uriToStringWindows(uri) : uri.toString(true);
}

// NOTE: default `uri.toString` converts windows drive letters to lower-case
// which causes issues with flow so below patching toString to return upper-case drive letters
// see issue https://github.com/flowtype/flow-for-vscode/issues/260
function uriToStringWindows(uri: Uri): string {
  const uriString = uri.toString(true);
  const windowsDriveLetter = getWindowsDriveLetter(uri);
  return windowsDriveLetter
    ? makeWindowsDriveLetterUppercase(uriString, windowsDriveLetter)
    : uriString;
}

function isAlphabet(charCode: number): boolean {
  return (
    (charCode >= CharCode.A && charCode <= CharCode.Z) ||
    (charCode >= CharCode.a && charCode <= CharCode.z)
  );
}

function getWindowsDriveLetter(uri: Uri): string | null {
  const { path } = uri;

  let driveLetter: string | null = null;
  // path of type '/c:/'
  if (
    path.length >= 3 &&
    path.charCodeAt(0) === CharCode.Slash &&
    path.charCodeAt(2) === CharCode.Colon
  ) {
    const code = path.charCodeAt(1);
    if (
      (code >= CharCode.A && code <= CharCode.Z) ||
      (code >= CharCode.a && code <= CharCode.z)
    ) {
      driveLetter = path.charAt(1);
    }
    // path of type 'c:/'
  } else if (path.length >= 2 && path.charCodeAt(1) === CharCode.Colon) {
    const code = path.charCodeAt(0);
    if (isAlphabet(code)) {
      driveLetter = path.charAt(0);
    }
  }

  return driveLetter;
}

function makeWindowsDriveLetterUppercase(
  uriString: string,
  windowsDriveLetter: string,
): string {
  const lowerCaseDriveLetter = windowsDriveLetter.toLowerCase();
  const upperCaseDriveLetter = windowsDriveLetter.toUpperCase();
  return uriString.replace(
    `/${lowerCaseDriveLetter}:`,
    `/${upperCaseDriveLetter}:`,
  );
}
