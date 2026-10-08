import * as React from 'react';

declare function viaComponentProps<P extends {...}>(
  props: React.ComponentProps<component(...P)>,
): P;

viaComponentProps({name: 'a'}); // ok
viaComponentProps({name: 'a'}).name as number; // error: string ~> number

declare function viaComponentPropsWithFoo<P extends {...}>(
  props: React.ComponentProps<component(foo: string, ...P)>,
): P;
viaComponentPropsWithFoo({foo: 'x', name: 'a'}); // ok

type DialogProps = {isShown: boolean, onHide: () => void};
type TModal<TModalProps> = component(
  ...{...$Exact<TModalProps>, ...Omit<DialogProps, 'isShown'>}
);
declare function viaModalProps<TModalProps>(
  props: Readonly<React.ComponentProps<TModal<TModalProps>>>,
): TModalProps;
viaModalProps({name: 'a', onHide: () => {}}).name as number; // error: 'a' ~> number
