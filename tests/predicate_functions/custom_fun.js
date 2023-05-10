declare var getPrototypeOf: Object$GetPrototypeOf;
function test(event: any) {
  for (const prop in getPrototypeOf(event)) {
    const property = event[prop]; // should not be affected by predicate function call
  }
}
