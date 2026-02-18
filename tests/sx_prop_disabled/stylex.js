// @flow
export type CompiledStyles = {+[string]: string};
export type StyleXArray<+T> = T | ReadonlyArray<StyleXArray<T>>;

declare function props(
  ...styles: ReadonlyArray<StyleXArray<?boolean | CompiledStyles>>
): {className: string, style: {[string]: string | number}};

declare function create<S: {+[string]: {+[string]: mixed}}>(styles: S): S;

declare export default {
  props: typeof props,
  create: typeof create,
};
