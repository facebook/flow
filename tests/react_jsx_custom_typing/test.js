declare const Com: C;

{
  <Com foo="">{1}{2}</Com> as 1; // missing React, jsx still checked
}

{
  const React = {};
  <Com foo="">{1}{2}</Com> as 1; // missing createElement, jsx still checked
}

{
  const React = { createElement: 'hi' };
  <Com foo="">{1}{2}</Com> as 1; // bad createElement, jsx still checked
}

{
  declare const React: {
    createElement: React$CustomJSXFactory;
  };
  <Com foo={3} bar="">{2}{3}</Com> as 4; // error
}
