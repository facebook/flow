// @flow

import React from 'react';

{
  React.useMutationEffect(); // Error: function requires another argument.
}

type CustomType = {|
  foo: string,
  bar: number,
|};

{
  let stringValue: string;
  let numericValue: number;
  let customValue: CustomType;

  const StringContext = React.createContext('hello');
  stringValue = React.useContext(StringContext); // Ok
  numericValue = React.useContext(StringContext); // Error: string is incompatible with number

  const InvalidContext: React$Context<CustomType> = React.createContext('hello'); // Error: inexact string is incompatible with exact CustomType

  const CustomContext: React$Context<CustomType> = React.createContext({
    foo: 'abc',
    bar: 123,
  });
  stringValue = React.useContext(CustomContext); // Error: CustomType is incompatible with string
  customValue = React.useContext(CustomContext); // Ok
}