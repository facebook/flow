class A extends React.Component {
  render(): React.Element<any> {}
}

class B extends React.Component {
  render(): React.Element<mixed> {}
}

class C extends React.Component {
  render(): React.Element<*> {}
}

class D extends React.Component {
  render(): React.Element<> {}
}

class E extends React.Component {
  render(): ?React.Element<any> {}
}

class F extends React.Component {
  render(): ?React.Element<mixed> {}
}

class G extends React.Component {
  render(): ?React.Element<*> {}
}

class H extends React.Component {
  render(): ?ReactElement<any> {}
}

class I extends React.Component {
  render(): ?ReactElement<mixed> {}
}

class J extends React.Component {
  render(): ?ReactElement<*> {}
}

class K extends React.Component {
  render(): ?React.Element<Config> {}
}

class L extends React.Component {
  render(): ?React.Element<> {}
}

class M extends React.Component {
  render(): Element<any> {}
}

class N extends React.Component {
  render(): ?Element<any> {}
}
