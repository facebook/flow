{
  <div />; // ok
  React; // error
  1 as React.Node;
  new HTMLAnchorElement() as React.Node; // error
}
{
  const React = require('react');
  <div /> as React.Node; // ok
  React; // error
  1 as React.Node;
  new HTMLAnchorElement() as React.Node; // error
}
