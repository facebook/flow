// @flow
export type CompiledStyles = {readonly [string]: string};
export type StyleXArray<out T> = T | ReadonlyArray<StyleXArray<T>>;

declare function props(
  ...styles: ReadonlyArray<StyleXArray<?boolean | CompiledStyles>>
): {className: string, style: {[string]: string | number}};

declare function create<S extends {readonly [string]: {readonly [string]: unknown}}>(styles: S): S;

declare export default {
  props: typeof props,
  create: typeof create,
};
