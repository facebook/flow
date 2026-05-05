type GenericComponentType<TLayoutProps extends {...}> = component<
  +TItem,
>(
  children: (item: TItem) => React.Node,
  items: ReadonlyArray<TItem>,
);


declare const GenericComponent: GenericComponentType<{}>;

const items: ReadonlyArray<{title: string}> = [];

component EmptyListExample() {
  return (
    <GenericComponent items={items}>
      {item => <span>{item.title}</span>}
    </GenericComponent>
  );
}
