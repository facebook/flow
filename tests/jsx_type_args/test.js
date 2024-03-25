import * as React from 'react';

declare function Component<T>({
  prop: T,
}): React.Node;

declare function ComponentWithChildren<T>({
  prop: T,
  children: React.Node,
}): React.Node;

declare function ComponentMultipleArgs<R, S, T: string>({
  r: R,
  s: S,
  t: T,
}): React.Node;

<Component<string> prop="foo" />;
<Component<_> prop="foo" />;
<ComponentWithChildren<string> prop="foo" ><div /></ComponentWithChildren>;
<ComponentMultipleArgs<string, number, boolean> r="foo" s={1} t={true} />; // error: bool ~> string
<ComponentMultipleArgs<string, number, boolean> r="foo" s={1} t="s" />; // error: bool ~> string, string ~> bool
<ComponentMultipleArgs<string> r="foo" s={1} t="s" />; // error: arity
