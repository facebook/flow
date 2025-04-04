declare class Element {}
declare class HTMLElement extends Element {}
declare class Event {}

type ReactDOM$HTMLElementProps = {
  id?: ?string,
  onClick?: ?(Event) => mixed,
};

type ReactDOM$divProps = ReactDOM$HTMLElementProps;

declare class HTMLDivElement extends HTMLElement {
  tagName: 'DIV';
}

type ReactDOM$divInstance = HTMLDivElement;

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
  [string]: ReactDOM$HTMLElementJSXIntrinsic,
  div: ReactDOM$HTMLElementJSXIntrinsicTyped<ReactDOM$divProps, ReactDOM$divInstance>,
}
