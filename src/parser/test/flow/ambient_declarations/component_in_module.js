declare module 'test-components' {
  component Button(props: { label: string, onClick: () => void }) renders React.Node;

  component Input(props: { value: string, onChange: (string) => void }) renders React.Node;

  component List<T>(items: Array<T>, renderItem: (T) => React.Node) renders React.Node;

  declare export { Button, Input, List };
}
