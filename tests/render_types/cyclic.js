type Bar = $ReadOnly<{}>;

type Props = $ReadOnly<{
  ...Bar;
  children: renders* C;
}>;

component C(...{children}: Props) {
  return children;
}
