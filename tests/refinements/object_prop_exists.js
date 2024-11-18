{
  declare const x: {|a: true, b: number|} | {|a: false, b: string, c: true|};
  if (x.c) {
    x as empty; // ERROR
    x as {|a: false, b: string, c: true|}; // OK
  } else {
    x as empty; // ERROR
    x as {|a: true, b: number|}; // OK
  }
}

{
  // Non-readable
  declare const x: {|a: true, b: number|} | {|a: false, b: string, -c: true|};
  if (x.c) { // ERROR: prop not readable
    x as empty; // ERROR
  } else {
    x as empty; // ERROR
  }
}

{
  function withResult1(result: {ok: true} | {ok: false}): string {
    if(result?.ok === false) {
        return result as empty; // good error: result not refined to empty
    }
    return "Hello"
  }
  function withResult2(result: {ok: true} | {ok: void}): string {
    if(result?.ok === undefined) {
        return result as empty; // good error: result not refined to empty
    }
    return "Hello"
  }
  function withResult3(result: {ok: true} | {ok: null}): string {
    if(result?.ok === null) {
        return result as empty; // bad: result is not empty
    }
    return "Hello"
  }
  function withResult4(result: {ok: true} | {ok: null}): string {
    if(result?.ok == null) {
        return result as empty; // good error: result not refined to empty
    }
    return "Hello"
  }
  function withResult5(result: {ok: true} | {ok: null}): string {
    if(result?.ok == undefined) {
        return result as empty; // good error: result not refined to empty
    }
    return "Hello"
  }
}
