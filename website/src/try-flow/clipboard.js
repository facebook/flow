/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

// Copy `text` to the clipboard, resolving to whether the copy succeeded.
//
// The async Clipboard API is the happy path, but it is unavailable in insecure
// contexts (http: on a non-localhost host) and rejects when the document isn't
// focused or when a containing iframe's permissions policy forbids
// clipboard-write (e.g. the playground embedded in staticdocs). In those cases
// we fall back to a hidden <textarea> + document.execCommand('copy').
export default function copyText(text: string): Promise<boolean> {
  if (navigator.clipboard != null) {
    return navigator.clipboard
      .writeText(text)
      .then(() => true)
      .catch(() => fallbackCopyText(text));
  }
  return Promise.resolve(fallbackCopyText(text));
}

function fallbackCopyText(text: string): boolean {
  const body = typeof document !== 'undefined' ? document.body : null;
  if (body == null) {
    return false;
  }
  const previouslyFocused: $FlowFixMe = document.activeElement;
  const textarea = document.createElement('textarea');
  textarea.value = text;
  textarea.setAttribute('readonly', '');
  // Keep it off-screen so selecting it doesn't scroll the page or flash.
  textarea.style.position = 'fixed';
  textarea.style.top = '-9999px';
  textarea.style.opacity = '0';
  body.appendChild(textarea);
  textarea.select();
  let ok = false;
  try {
    ok = document.execCommand('copy');
  } catch {
    ok = false;
  }
  body.removeChild(textarea);
  // Selecting the textarea steals focus; restore it so keyboard users stay put.
  if (
    previouslyFocused != null &&
    typeof previouslyFocused.focus === 'function'
  ) {
    previouslyFocused.focus();
  }
  return ok;
}
