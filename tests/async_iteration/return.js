declare const gen: AsyncGenerator<void,string,void>;

gen.return('done');
gen.return(Promise.resolve('done'));
gen.return(0).then(result => { // ERROR: number is not the generator's return type
  result.value as void; // ERROR: string ~> void
});

// However, a generator can "refuse" the return by catching an exception and
// yielding or returning internally.
async function *refuse_return() {
  try {
    yield 1;
  } finally {
    return 0;
  }
}
refuse_return().return("string").then(result => { // ERROR: string is not the generator's return type
  if (result.done) {
    result.value as string; // error: number | void ~> string
  }
});
