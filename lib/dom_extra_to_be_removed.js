/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

interface ImportMeta {
  url?: string;
}

/* Commonly available, shared between node and dom */
declare var console: {
    assert(condition: unknown, ...data: Array<any>): void,
    clear(): void,
    count(label?: string): void,
    countReset(label?: string): void,
    debug(...data: Array<any>): void,
    dir(...data: Array<any>): void,
    dirxml(...data: Array<any>): void,
    error(...data: Array<any>): void,
    _exception(...data: Array<any>): void,
    group(...data: Array<any>): void,
    groupCollapsed(...data: Array<any>): void,
    groupEnd(): void,
    info(...data: Array<any>): void,
    log(...data: Array<any>): void,
    profile(name?: string): void,
    profileEnd(name?: string): void,
    table(tabularData: { [key: string]: any, ... } | Array<{ [key: string]: any, ... }> | Array<Array<any>>): void,
    time(label?: string): void,
    timeEnd(label: string): void,
    timeStamp(label?: string): void,
    timeLog(label?: string, ...data?: Array<any>): void,
    trace(...data: Array<any>): void,
    warn(...data: Array<any>): void,
    ...
};

declare function btoa(rawString: string): string;
declare function atob(encodedString: string): string;
declare function clearInterval(intervalId: ?IntervalID): void;
declare function clearTimeout(timeoutId: ?TimeoutID): void;
declare function setTimeout<TArguments extends Array<unknown>>(
  callback: (...args: TArguments) => unknown,
  ms?: number,
  ...args: TArguments
): TimeoutID;
declare function setInterval<TArguments extends Array<unknown>>(
  callback: (...args: TArguments) => unknown,
  ms?: number,
  ...args: TArguments
): IntervalID;
declare function queueMicrotask<TArguments extends ReadonlyArray<unknown>>(
  callback: (...args: TArguments) => unknown,
): void;
interface VoidFunction {
    (): void;
}

declare opaque type TimeoutID;
declare opaque type IntervalID;

/* Bare-bone stand-in for csstype, which only ships with the default builtin lib */
declare namespace React$CSS {
  type Properties<TLength = string | 0, TTime = string> = {readonly [property: string]: any};
}

/* Bare-bone stand-ins for the DOM interfaces react.js uses, which only ship with the default builtin lib */
interface Element {}
interface Event {}
interface EventTarget {}
interface UIEvent {}
interface MouseEvent {}

/* Bare-bone stand-ins for the React DOM declarations, which only ship with the default builtin lib */
declare namespace React {
  type BaseSyntheticEvent<E = object, C = any, T = any> = {readonly [property: string]: any};
  type SyntheticEvent<T = Element, E = Event> = BaseSyntheticEvent<E, EventTarget & T, EventTarget>;
  type AnimationEvent<T = Element> = SyntheticEvent<T>;
  type ChangeEvent<CurrentTarget = Element, Target = Element> = SyntheticEvent<CurrentTarget>;
  type ClipboardEvent<T = Element> = SyntheticEvent<T>;
  type CompositionEvent<T = Element> = SyntheticEvent<T>;
  type FocusEvent<Target = Element, RelatedTarget = Element> = SyntheticEvent<Target>;
  type FormEvent<T = Element> = SyntheticEvent<T>;
  type InputEvent<T = Element> = SyntheticEvent<T>;
  type InvalidEvent<T = Element> = SyntheticEvent<T>;
  type SubmitEvent<T = Element> = SyntheticEvent<T>;
  type ToggleEvent<T = Element> = SyntheticEvent<T>;
  type TransitionEvent<T = Element> = SyntheticEvent<T>;
  type UIEvent<T = Element, E = globalThis.UIEvent> = SyntheticEvent<T, E>;
  type KeyboardEvent<T = Element> = UIEvent<T>;
  type MouseEvent<T = Element, E = globalThis.MouseEvent> = UIEvent<T, E>;
  type TouchEvent<T = Element> = UIEvent<T>;
  type DragEvent<T = Element> = MouseEvent<T>;
  type PointerEvent<T = Element> = MouseEvent<T>;
  type WheelEvent<T = Element> = MouseEvent<T>;

  type EventHandler<E> = (event: E) => void;
  type ReactEventHandler<T = Element> = EventHandler<SyntheticEvent<T>>;
  type AnimationEventHandler<T = Element> = EventHandler<AnimationEvent<T>>;
  type ChangeEventHandler<CurrentTarget = Element, Target = Element> = EventHandler<ChangeEvent<CurrentTarget, Target>>;
  type ClipboardEventHandler<T = Element> = EventHandler<ClipboardEvent<T>>;
  type CompositionEventHandler<T = Element> = EventHandler<CompositionEvent<T>>;
  type DragEventHandler<T = Element> = EventHandler<DragEvent<T>>;
  type FocusEventHandler<T = Element> = EventHandler<FocusEvent<T>>;
  type FormEventHandler<T = Element> = EventHandler<FormEvent<T>>;
  type InputEventHandler<T = Element> = EventHandler<InputEvent<T>>;
  type KeyboardEventHandler<T = Element> = EventHandler<KeyboardEvent<T>>;
  type MouseEventHandler<T = Element> = EventHandler<MouseEvent<T>>;
  type PointerEventHandler<T = Element> = EventHandler<PointerEvent<T>>;
  type SubmitEventHandler<T = Element> = EventHandler<SubmitEvent<T>>;
  type ToggleEventHandler<T = Element> = EventHandler<ToggleEvent<T>>;
  type TouchEventHandler<T = Element> = EventHandler<TouchEvent<T>>;
  type TransitionEventHandler<T = Element> = EventHandler<TransitionEvent<T>>;
  type UIEventHandler<T = Element> = EventHandler<UIEvent<T>>;
  type WheelEventHandler<T = Element> = EventHandler<WheelEvent<T>>;

  type AbstractView = {readonly [property: string]: any};
  type Touch = {readonly [property: string]: any};
  type TouchList = {readonly [property: string]: any};
  type ModifierKey = string;
  type CSSProperties = React$CSS.Properties<string | number>;

  type AriaAttributes = {readonly [property: string]: any};
  type AriaRole = string;
  type DOMAttributes<T> = {readonly [property: string]: any};
  type HTMLAttributes<T> = {readonly [property: string]: any};
  type AllHTMLAttributes<T> = HTMLAttributes<T>;
  type AnchorHTMLAttributes<T> = HTMLAttributes<T>;
  type AreaHTMLAttributes<T> = HTMLAttributes<T>;
  type AudioHTMLAttributes<T> = HTMLAttributes<T>;
  type BaseHTMLAttributes<T> = HTMLAttributes<T>;
  type BlockquoteHTMLAttributes<T> = HTMLAttributes<T>;
  type ButtonHTMLAttributes<T> = HTMLAttributes<T>;
  type CanvasHTMLAttributes<T> = HTMLAttributes<T>;
  type ColHTMLAttributes<T> = HTMLAttributes<T>;
  type ColgroupHTMLAttributes<T> = HTMLAttributes<T>;
  type DataHTMLAttributes<T> = HTMLAttributes<T>;
  type DelHTMLAttributes<T> = HTMLAttributes<T>;
  type DetailsHTMLAttributes<T> = HTMLAttributes<T>;
  type DialogHTMLAttributes<T> = HTMLAttributes<T>;
  type EmbedHTMLAttributes<T> = HTMLAttributes<T>;
  type FieldsetHTMLAttributes<T> = HTMLAttributes<T>;
  type FormHTMLAttributes<T> = HTMLAttributes<T>;
  type HtmlHTMLAttributes<T> = HTMLAttributes<T>;
  type IframeHTMLAttributes<T> = HTMLAttributes<T>;
  type ImgHTMLAttributes<T> = HTMLAttributes<T>;
  type InputHTMLAttributes<T> = HTMLAttributes<T>;
  type InsHTMLAttributes<T> = HTMLAttributes<T>;
  type KeygenHTMLAttributes<T> = HTMLAttributes<T>;
  type LabelHTMLAttributes<T> = HTMLAttributes<T>;
  type LiHTMLAttributes<T> = HTMLAttributes<T>;
  type LinkHTMLAttributes<T> = HTMLAttributes<T>;
  type MapHTMLAttributes<T> = HTMLAttributes<T>;
  type MediaHTMLAttributes<T> = HTMLAttributes<T>;
  type MenuHTMLAttributes<T> = HTMLAttributes<T>;
  type MetaHTMLAttributes<T> = HTMLAttributes<T>;
  type MeterHTMLAttributes<T> = HTMLAttributes<T>;
  type ObjectHTMLAttributes<T> = HTMLAttributes<T>;
  type OlHTMLAttributes<T> = HTMLAttributes<T>;
  type OptgroupHTMLAttributes<T> = HTMLAttributes<T>;
  type OptionHTMLAttributes<T> = HTMLAttributes<T>;
  type OutputHTMLAttributes<T> = HTMLAttributes<T>;
  type ParamHTMLAttributes<T> = HTMLAttributes<T>;
  type ProgressHTMLAttributes<T> = HTMLAttributes<T>;
  type QuoteHTMLAttributes<T> = HTMLAttributes<T>;
  type ScriptHTMLAttributes<T> = HTMLAttributes<T>;
  type SelectHTMLAttributes<T> = HTMLAttributes<T>;
  type SlotHTMLAttributes<T> = HTMLAttributes<T>;
  type SourceHTMLAttributes<T> = HTMLAttributes<T>;
  type StyleHTMLAttributes<T> = HTMLAttributes<T>;
  type TableHTMLAttributes<T> = HTMLAttributes<T>;
  type TdHTMLAttributes<T> = HTMLAttributes<T>;
  type TextareaHTMLAttributes<T> = HTMLAttributes<T>;
  type ThHTMLAttributes<T> = HTMLAttributes<T>;
  type TimeHTMLAttributes<T> = HTMLAttributes<T>;
  type TrackHTMLAttributes<T> = HTMLAttributes<T>;
  type VideoHTMLAttributes<T> = HTMLAttributes<T>;
  type WebViewHTMLAttributes<T> = HTMLAttributes<T>;
  type HTMLProps<T> = AllHTMLAttributes<T>;
  type DetailedHTMLProps<E, T> = E;
  type SVGAttributes<T> = {readonly [property: string]: any};
  type SVGProps<T> = SVGAttributes<T>;
  type SVGLineElementAttributes<T> = SVGProps<T>;
  type SVGTextElementAttributes<T> = SVGProps<T>;

  type HTMLElementType = string;
  type SVGElementType = string;
  type HTMLAttributeAnchorTarget = string;
  type HTMLAttributeReferrerPolicy = string;
  type HTMLInputTypeAttribute = string;
  type HTMLInputAutoCompleteAttribute = string;
  type AutoFill = string;
  type AutoFillAddressKind = string;
  type AutoFillBase = string;
  type AutoFillContactField = string;
  type AutoFillContactKind = string;
  type AutoFillCredentialField = string;
  type AutoFillField = string;
  type AutoFillNormalField = string;
  type AutoFillSection = string;
  type OptionalPrefixToken<T extends string> = string;
  type OptionalPostfixToken<T extends string> = string;
  type DO_NOT_USE_OR_YOU_WILL_BE_FIRED_EXPERIMENTAL_FORM_ACTIONS = {...};
  type DO_NOT_USE_OR_YOU_WILL_BE_FIRED_EXPERIMENTAL_IMG_SRC_TYPES = {...};
  type DO_NOT_USE_OR_YOU_WILL_BE_FIRED_EXPERIMENTAL_MEDIA_SRC_TYPES = {...};

  declare namespace JSX {
    interface IntrinsicElements {
      [elemName: string]: any;
    }
  }
}
