{
  <div />; // ok
  React; // error
  1 as React.Node;
  Array as React.Node; // error
}
{
  const React = require('react');
  <div /> as React.Node; // ok
  React; // error
  1 as React.Node;
  Array as React.Node; // error
}
