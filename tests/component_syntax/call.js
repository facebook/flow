component Foo() {
    return null;
}
// $FlowFixMe
Foo(); // ERROR, even with fixme

// $FlowFixMe[react-rule-call-component]
Foo(); // OK with specific fixme
