declare namespace ComponentNamespace {
  component MyComponent(props: { name: string }) renders React.Node;

  component AnotherComponent(data: number) renders React.Element<any>;

  component GenericComponent<T>(value: T) renders React.Node;

  declare function helperFn(): void;
}

export { ComponentNamespace };
