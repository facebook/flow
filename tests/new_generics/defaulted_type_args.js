type ContextBox<TItem, TColumnKey> = {
  column: TColumnKey,
  item: TItem,
};

declare function contextualInner<TItem, TColumnKey = string>(): ContextBox<
  TItem,
  TColumnKey,
>;

function contextualOuter<TItem, TColumnKey>(): ContextBox<TItem, TColumnKey> {
  return contextualInner();
}

contextualOuter<number, number>() as ContextBox<number, number>;

type UnderscoreBox<TItem, TColumnKey> = {
  column: TColumnKey,
  item: TItem,
};

declare function underscoreInner<TItem, TColumnKey = string>(): UnderscoreBox<
  TItem,
  TColumnKey,
>;

const underscoreBox: UnderscoreBox<number, number> = underscoreInner<number, _>();

underscoreBox.column as number;

type Plugin<TItem, TColumnKey> = {
  transformBodyCell?: (column: TColumnKey, item: TItem) => void,
};

declare function siblingPlugin<TItem, TColumnKey>(): Plugin<TItem, TColumnKey>;
declare function rowInteraction<TItem, TColumnKey = string>(): Plugin<
  TItem,
  TColumnKey,
>;

function collectPlugins<TItem, TColumnKey>(): Array<Plugin<TItem, TColumnKey>> {
  const plugins = [
    siblingPlugin<TItem, TColumnKey>(),
    rowInteraction<TItem, _>(),
  ];
  return plugins; // ERROR
}

type LateBox<TItem, TColumnKey> = {
  column: TColumnKey,
  item: TItem,
};

declare function lateInner<TItem, TColumnKey = string>(): LateBox<
  TItem,
  TColumnKey,
>;

function collectBoxes<TItem, TColumnKey>(): Array<LateBox<TItem, TColumnKey>> {
  const box = lateInner<TItem, _>();
  return [box]; // ERROR
}

collectBoxes<number, number>() as Array<LateBox<number, number>>;
