{
  <div /> as ExactReactElement_DEPRECATED<'span'>; // missing React, jsx still checked
}

{
  const React = {};
  <div /> as ExactReactElement_DEPRECATED<'span'>; // missing createElement, jsx still checked
}

{
  const React = { createElement: 'hi' };
  <div /> as ExactReactElement_DEPRECATED<'span'>; // bad createElement, jsx still checked
}
