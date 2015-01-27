// No indexer should be fine
function foo(): {} {
  return { foo: "bar" }
}

// Matching indexer should be fine
function foo(): {[key: string]: string} {
  return { foo: "bar" }
}

// Indexer with different key type is an error when it matches
function foo(): {[key: number]: string} {
  return { foo: "bar" }
}

// Matching indexer with different value type is an error
function foo(): {[key: string]: number} {
  return { foo: "bar" }
}

// Indexer with different key type and different value type is twice an error
function foo(): {[key: number]: number} {
  return { foo: "bar" }
}

// If key exists in object type then indexer is not matched
function foo(): {[key: string]: number; foo: string} {
  return { foo: "bar" }
}

// If key exists in object type then indexer is not matched
function foo(): {[key: number]: number; foo: string} {
  return { foo: "bar" }
}

// Should still complain about mistyped properties
function foo(): {[key: string]: number; foo: number} {
  return { foo: "bar" }
}
