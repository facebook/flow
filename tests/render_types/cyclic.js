type Bar = Readonly<{}>;

type Props = Readonly<{
  ...Bar;
  children: renders* C;
}>;

component C(...{children}: Props) {
  return children;
}
