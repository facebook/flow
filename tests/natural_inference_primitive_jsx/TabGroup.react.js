import typeof TabType from './Tab.react';

import * as React from 'react';

export type Props<TValue> = $ReadOnly<{
  children: React.ChildrenArray<?ExactReactElement_DEPRECATED<TabType<TValue>>>,
  onChange: (value: TValue) => mixed,
  value?: ?TValue,
}>;

declare component TabGroup<TValue>(...props: Props<TValue>);
declare export default (component<TValue>(...props: Props<TValue>));
