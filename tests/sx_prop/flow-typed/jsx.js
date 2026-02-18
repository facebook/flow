declare class Element {}
declare class HTMLElement extends Element {}

type ReactDOM$HTMLElementProps = {
  className?: string,
  style?: {[string]: string | number},
};

type ReactDOM$divProps = ReactDOM$HTMLElementProps;
type ReactDOM$spanProps = ReactDOM$HTMLElementProps;

declare class HTMLDivElement extends HTMLElement {}
declare class HTMLSpanElement extends HTMLElement {}

type ReactDOM$HTMLElementJSXIntrinsicTyped<Props, Instance = HTMLElement> = {
  instance: Instance,
  props: Props,
};

type ReactDOM$HTMLElementJSXIntrinsic = {
  instance: HTMLElement,
  props: {
    +[key: string]: any,
    +children?: React$Node,
    ...
  },
  ...
};

declare type $JSXIntrinsics = {
  div: ReactDOM$HTMLElementJSXIntrinsicTyped<ReactDOM$divProps, HTMLDivElement>,
  span: ReactDOM$HTMLElementJSXIntrinsicTyped<ReactDOM$spanProps, HTMLSpanElement>,
}
