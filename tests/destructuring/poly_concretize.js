/**
 * Regression test: ConcretizeForDestructuring must not instantiate PolyT
 * types during concretization (the PolyT catch-all handler should not
 * intercept destructuring concretization).
 */

// A union type used in object type spreads
type LabelProps =
  | {label: string, labelledby?: void, ...}
  | {label?: void, labelledby: string, ...};

type Base = {
  name: string,
  id: string,
 ...};

// PressableItem includes both Base and LabelProps via spread
type PressableItem = {
  ...Base,
  onPress?: () => void,
  linkProps?: {href: string, ...},
  ...LabelProps,
 ...};

type NonPressableItem = {
  ...Base,
 ...};

type Item = PressableItem | NonPressableItem;

// Generic function accepting an item and its additionalOptions
function mapItems<T: {...}, TKey: string>(
  items: Array<T>,
  additionalOptions: {
    filter?: (item: T) => boolean,
 ...  },
): Array<T> {
  return items;
}

// Test: destructuring with type annotation on a union with spreads
// then passing the destructured value to a generic function
function test(items: Array<Item>) {
  const additionalOptions = {};
  return mapItems<Item, string>(
    items,
    {
      ...additionalOptions,
      customErrorPrefix: 'test',
    },
  );
}

// Test: rest pattern on a union type with spread annotation
function testRest(item: Item): void {
  const {
    name,
    id,
    ...rest
  }: {
    ...Item,
    name?: string,
 ...  } = item;
  // Accessing a property that only exists on one branch of the union
  // should produce an error, but should NOT produce a different error
  // than the baseline.
}

// Test: destructured value used in switch (should preserve full type)
type Env = 'A' | 'B' | 'C';
type Config = {env: Env, ...};

function testSwitch(config: Config): void {
  const {env}: Config = config;
  switch (env) {
    case 'A':
      break;
    case 'B':
      break;
    case 'C':
      break;
  }
}
