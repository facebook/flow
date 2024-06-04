function Foo(_props: {foo: string}) {}
Foo.defaultProps = { foo: 'bar' };

<Foo />; // error: missing foo prop
const props: React.ElementConfig<typeof Foo> = {}; // error: missing foo prop
