component Foo(otherRef: { current: HTMLElement | null }) {
    otherRef.current; // error
    return null;
}

component Bar(notReallyARef: { current: string, next: string }) {
    notReallyARef.current; // no error
    return null;
}
