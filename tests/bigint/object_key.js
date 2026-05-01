type T = {
  "1": string,
}

({ 1n: "foo" }) as T; // error (TODO: non-string literals not supported)
({ 2n: "foo" }) as T; // error
