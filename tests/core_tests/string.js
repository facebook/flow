// @flow

let tests = [
  // incorrect use of split
  function() {
    const parts = "foo,bar".split('.');
    parts[0] = parts[0].replace(',', ''); // invalid, we don't know this is a string
    return parts.join(",");
  },

  // basic use of split with guard
  function() {
    const parts = "foo,bar".split('.');
    if (parts[0]) parts[0] = parts[0].replace(',', ''); 
    return parts.join(",");
  },

];
