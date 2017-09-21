/**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

declare module 'react-dom' {
  declare function findDOMNode(
    componentOrElement : Element | ?React$Component<any, any>,
  ): null | Element | Text;

  declare function render<ElementType: React$ElementType>(
    element: React$Element<ElementType>,
    container: Element,
    callback?: () => void,
  ): React$ElementRef<ElementType>;

  declare function createPortal(
    node: React$Node,
    container: Element,
  ): React$Portal;

  declare function unmountComponentAtNode(container: any): boolean;
  declare var version: string;

  declare function unstable_batchedUpdates<A, B, C, D, E>(
    callback: (a: A, b: B, c: C, d: D, e: E) => mixed,
    a: A,
    b: B,
    c: C,
    d: D,
    e: E
  ): void;
  declare function unstable_renderSubtreeIntoContainer<
    ElementType: React$ElementType,
  >(
    parentComponent: React$Component<any, any>,
    nextElement: React$Element<ElementType>,
    container: any,
    callback?: () => void
  ): React$ElementRef<ElementType>;
}

declare module 'react-dom/server' {
  declare function renderToString(element: React$Node): string;
  declare function renderToStaticMarkup(element: React$Node): string;
}

declare class SyntheticEvent<+T: EventTarget = EventTarget> {
  bubbles: boolean;
  cancelable: boolean;
  +currentTarget: T;
  defaultPrevented: boolean;
  eventPhase: number;
  isDefaultPrevented(): boolean;
  isPropagationStopped(): boolean;
  isTrusted: boolean;
  nativeEvent: Event;
  preventDefault(): void;
  stopPropagation(): void;
  // This should not be `T`. Use `currentTarget` instead. See:
  // https://github.com/DefinitelyTyped/DefinitelyTyped/issues/11508#issuecomment-256045682
  +target: EventTarget;
  timeStamp: number;
  type: string;
  persist(): void;
}

declare class SyntheticAnimationEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  animationName: string;
  elapsedTime: number;
  pseudoElement: string;
}

declare class SyntheticClipboardEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  clipboardData: any;
}

declare class SyntheticCompositionEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  data: any;
}

declare class SyntheticInputEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  +target: HTMLInputElement;
  data: any;
}

declare class SyntheticUIEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  detail: number;
  view: any;
}

declare class SyntheticFocusEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T> {
  relatedTarget: EventTarget;
}

declare class SyntheticKeyboardEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T> {
  altKey: boolean;
  charCode: number;
  ctrlKey: boolean;
  getModifierState: any;
  key: string;
  keyCode: number;
  locale: string;
  location: number;
  metaKey: boolean;
  repeat: boolean;
  shiftKey: boolean;
  which: number;
}

declare class SyntheticMouseEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T> {
  altKey: boolean;
  button: number;
  buttons: number;
  clientX: number;
  clientY: number;
  ctrlKey: boolean;
  getModifierState: any;
  metaKey: boolean;
  pageX: number;
  pageY: number;
  relatedTarget: EventTarget;
  screenX: number;
  screenY: number;
  shiftKey: boolean;
}

declare class SyntheticDragEvent<+T: EventTarget = EventTarget>
  extends SyntheticMouseEvent<T> {
  dataTransfer: any;
}

declare class SyntheticWheelEvent<+T: EventTarget = EventTarget>
  extends SyntheticMouseEvent<T> {
  deltaMode: number;
  deltaX: number;
  deltaY: number;
  deltaZ: number;
}

declare class SyntheticTouchEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T> {
  altKey: boolean;
  changedTouches: any;
  ctrlKey: boolean;
  getModifierState: any;
  metaKey: boolean;
  shiftKey: boolean;
  targetTouches: any;
  touches: any;
}

declare class SyntheticTransitionEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  propertyName: string;
  elapsedTime: number;
  pseudoElement: string;
}

declare type $JSXIntrinsics = {
  [string]: {
    instance: any,
    props: {
      children?: React$Node,
      [key: string]: any,
    },
  },
};
