type T = {
  "1": string,
}

({ 1n: "foo" }: T); // error (TODO: non-string literals not supported)
({ 2n: "foo" }: T); // error
