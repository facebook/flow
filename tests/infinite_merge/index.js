// @flow

import * as React from "react";

type List = $ReadOnlyArray<{
  name: string,
  list?: List
}>;

type Props = {
  list: List
};

const flatMap = (list, fn) => list.reduce((acc, d) => [...acc, ...fn(d)], []);

const flattenTree = tree => {
  const flattenTreeImpl = (tree, level) =>
    flatMap(tree, item => {
      const newItem = { level };
      if (item.list) {
        return [newItem, ...flattenTreeImpl(item.list, level + 1)];
      } else {
        return [newItem];
      }
    })

  return flattenTreeImpl(tree, 0);
};

export const Tree = (props: Props) => flattenTree(props.list)
