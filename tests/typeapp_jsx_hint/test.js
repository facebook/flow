type GenericComponentType<TLayoutProps: {...}> = component<
  +TItem,
>(
  children: (item: TItem) => React.Node,
  items: $ReadOnlyArray<TItem>,
);


declare const GenericComponent: GenericComponentType<{}>;

const items: $ReadOnlyArray<{title: string}> = [];

component EmptyListExample() {
  return (
    <GenericComponent items={items}>
      {item => <span>{item.title}</span>}
    </GenericComponent>
  );
}
