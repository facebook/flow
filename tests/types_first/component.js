export component ComponentWithTypeof(bar: string, baz: typeof bar, boz: {boz: typeof baz}) { return } // error: name-not-bound
