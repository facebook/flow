/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

declare class CSSStyleSheet {
  cssRules: CSSRuleList;
  ownerRule: ?CSSRule;
  deleteRule(index: number): void;
  insertRule(rule: string, index: number): void;
}

declare class CSSRule {
  cssText: string;
  parentRule: ?CSSRule;
  parentStyleSheet: ?CSSStyleSheet;
  type: number;
  static STYLE_RULE: number;
  static MEDIA_RULE: number;
  static FONT_FACE_RULE: number;
  static PAGE_RULE: number;
  static IMPORT_RULE: number;
  static CHARSET_RULE: number;
  static UNKNOWN_RULE: number;
  static KEYFRAMES_RULE: number;
  static KEYFRAME_RULE: number;
  static NAMESPACE_RULE: number;
  static COUNTER_STYLE_RULE: number;
  static SUPPORTS_RULE: number;
  static DOCUMENT_RULE: number;
  static FONT_FEATURE_VALUES_RULE: number;
  static VIEWPORT_RULE: number;
  static REGION_STYLE_RULE: number;
}

declare class CSSRuleList {
  length: number;
  item(index: number): ?CSSRule;
}

declare class CSSStyleDeclaration {
  /* DOM CSS Properties */
  background: string;
  visibility: string;

  cssFloat: string;
  cssText: string;
  getPropertyPriority(property: string): string;
  getPropertyValue(property:string): string;
  item(index: number): string;
  length: number;
  parentRule: CSSRule;
  removeProperty(property: string): string;
  setProperty(property: string, value: ?string, priority: ?string): void;
  setPropertyPriority(property: string, priority: string): void;
  setPropertyValue(property: string, value: string): void;
}
