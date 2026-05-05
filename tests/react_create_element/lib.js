type PropsFromPropsArgAndChildrenArg<
  PropsArg extends {...},
  ChildrenArg extends ReadonlyArray<unknown>,
> = ChildrenArg extends []
  ? PropsArg
  : ChildrenArg extends [+onlyChild: infer OnlyChild]
    ? {|...$Exact<PropsArg>, children: OnlyChild|}
    : {|...$Exact<PropsArg>, children: ChildrenArg|};

type ExpectedProps<Comp extends React$ElementType> = Readonly<{|
  ...$Exact<React$ElementConfig<Comp>>,
  key?: React$Key,
  ref?: ?React.RefSetter<React.ElementRef<Comp>>,
|}>;

type CheckedReactElement<
  Comp extends React$ElementType,
  Props extends {...},
  Children extends ReadonlyArray<unknown>,
  // _Validation is never going to be constrained, so it will always take the default.
  // Then we can force it to be compatible with the expected config with the type parameter bound check.
  // The check will fail under the generic version, so we suppress
  _ValidatedProps extends ExpectedProps<Comp> = PropsFromPropsArgAndChildrenArg<
    Props,
    Children,
  >,
> = ExactReactElement_DEPRECATED<Comp>;

declare opaque type React$CreateElement: (< // intentional-libdef-override
  Comp extends React$ElementType,
  Children extends ReadonlyArray<unknown> = [],
>(
  component: Comp,
  props?: null | void,
  ...children: Children
) => CheckedReactElement<Comp, {||}, Children>) &
  (<
    Comp extends React$ElementType,
    Props extends {...},
    Children extends ReadonlyArray<unknown> = [],
  >(
    component: Comp,
    props: Props,
    ...children: Children
  ) => CheckedReactElement<Comp, Props, Children>);
