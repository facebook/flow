import * as React from 'react';

declare export function compose<A, B, R>(
  f1: (a: A) => R,
  f2: (b: B) => A,
): (B) => R;

export function mapProps<InputProps: {...}, OutputProps: {...}>(
  mapperFn: (InputProps) => OutputProps,
): (component(...OutputProps)) => component(...InputProps) {
  return Component => props => <Component {...mapperFn(props)} />;
}

export function withProps<Props: {...}, ExtraProps: {...}>(
  extraFn: (Props) => $Exact<ExtraProps>,
): component(...{|
  ...$Exact<Props>,
  ...$Exact<ExtraProps>,
|}) => component(...$Exact<Props>) {
  return Component => props => <Component {...props} {...extraFn(props)} />;
}
