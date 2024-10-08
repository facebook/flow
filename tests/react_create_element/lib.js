type PropsFromPropsArgAndChildrenArg<
  PropsArg: {...},
  ChildrenArg: $ReadOnlyArray<mixed>,
> =
  ChildrenArg extends [] ? PropsArg :
  ChildrenArg extends [+onlyChild: infer OnlyChild] ? {|...$Exact<PropsArg>, children: OnlyChild|} :
  {|...$Exact<PropsArg>, children: ChildrenArg|}

type ExpectedProps<Comp: React$ElementType> = $ReadOnly<{|
  ...$Exact<React$ElementConfig<Comp>>,
  key?: React$Key,
  ref?: ?React$RefSetter<React$ElementRef<Comp>>,
|}>;

type CheckedReactElement<
  Comp: React$ElementType,
  Props: {...},
  Children: $ReadOnlyArray<mixed>,
  // _Validation is never going to be constrained, so it will always take the default.
  // Then we can force it to be compatible with the expected config with the type parameter bound check.
  // The check will fail under the generic version, so we suppress
  // $FlowFixMe
  _ValidatedProps: ExpectedProps<Comp> = PropsFromPropsArgAndChildrenArg<Props, Children>
> =
  ExactReactElement_DEPRECATED<Comp>;

declare opaque type React$CreateElement:
  & (<
    Comp: React$ElementType,
    Children: $ReadOnlyArray<mixed> = [],
  >(component: Comp, props?: null | void, ...children: Children) => CheckedReactElement<Comp, {||}, Children>
  )
  & (<
    Comp: React$ElementType,
    Props: {...},
    Children: $ReadOnlyArray<mixed> = [],
  >(component: Comp, props: Props, ...children: Children) => CheckedReactElement<Comp, Props, Children>
  )
