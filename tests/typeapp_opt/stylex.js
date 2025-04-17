// minimized from https://github.com/facebook/stylex/blob/10501b354a02776dcb55229ed1bae6aef54875c0/packages/stylex/src/StyleXTypes.js#L69-L94
import type {StyleXClassNameFor, StyleXVar} from './opaque';

export type MapNamespaces<+S: {+[string]: mixed}> = {
  +[Key in keyof S]: S[Key] extends (...args: infer Args) => infer Obj
    ? (...args: Args) => $ReadOnly<[MapNamespace<Obj>, InlineStyles]>
    : MapNamespace<S[Key]>,
};

export type MapNamespace<+CSS: {+[string]: mixed}> = {
  +[Key in keyof CSS]: StyleXClassNameFor<Key, ComplexStyleValueType<CSS[Key]>>,
};

export type InlineStyles = $ReadOnly<{
  // $$css?: void,
  $$inline: true,
  [key: string]: string | number,
}>;

type ComplexStyleValueType<+T> =
  T extends StyleXVar<infer U>
    ? U
    : T extends string | number | null
      ? T
      : T extends $ReadOnlyArray<infer U>
        ? ComplexStyleValueType<U>
        : T extends { +default: infer A, +[string]: infer B }
          ? ComplexStyleValueType<A> | ComplexStyleValueType<B>
          : $ReadOnly<T>;


declare const x: MapNamespaces<{+item: {+hover: {+backgroundColor: '#f9f2ec'}}}>;
x as MapNamespaces<{item: {hover: {backgroundColor: '#f9f2ec'}}}>; // okay
