{
  <div /> as React$Element<'span'>; // missing React, jsx still checked
}

{
  const React = {};
  <div /> as React$Element<'span'>; // missing createElement, jsx still checked
}

{
  const React = { createElement: 'hi' };
  <div /> as React$Element<'span'>; // bad createElement, jsx still checked
}
