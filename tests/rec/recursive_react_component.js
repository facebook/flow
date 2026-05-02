// Array#concat
'use strict';
import * as React from 'react';

type ElementProp<
  T: React.ElementType,
  P: keyof React.ElementConfig<T>,
> = React.ElementConfig<T>[P];

type BaseProps = Readonly<{
  theme?: ElementProp<typeof TimeoutFn, 'theme'>,
  size?: ElementProp<typeof TimeoutFn, 'size'>,
}>;

type Props = {...BaseProps} | {...BaseProps};

const TimeoutFn = (props: Props) => {
  const tvarprops = props;
  return <DoesntMatter {...tvarprops} />;
};
