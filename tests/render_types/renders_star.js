import * as React from 'react';
component MenuItem() { return null }
(null: renders* MenuItem); // OK
(false: renders* MenuItem); // OK
(undefined: renders* MenuItem); // OK
([null, [false, [undefined]]]: renders* MenuItem); // OK
([null, false, undefined]: renders* MenuItem); // OK
(true: renders* MenuItem); // ERROR

(<MenuItem />: renders* MenuItem); // OK
declare const menuItemChildrenArray: React.ChildrenArray<MenuItem>;
(menuItemChildrenArray: renders* MenuItem); // OK

component BlueMenuItem() renders MenuItem { return <MenuItem />; }
(<BlueMenuItem />: renders* MenuItem); // OK

declare const rendersMaybeBlueMenuItem: renders? MenuItem;
(rendersMaybeBlueMenuItem: renders* MenuItem); // OK

declare const rendersChildrenArrayBlueMenuItem: renders? React.ChildrenArray<BlueMenuItem>; // invalid-render
(rendersChildrenArrayBlueMenuItem: renders* MenuItem); // OK

component Bad() { return null }
declare const rendersHuhBad: RendersHuh<Bad>;
(rendersHuhBad: renders* MenuItem); // ERROR

declare const rendersStarMenuItem: renders* MenuItem;
(rendersStarMenuItem: renders (null | false | void | MenuItem | $ReadOnlyArray<renders* MenuItem>)); // type checks, but invalid-render

([<MenuItem />, <BlueMenuItem />, <Bad />]: renders* MenuItem); // ERROR

component MenuItems() renders* MenuItem {
  return null; // OK
}

component Menu(children: renders* MenuItem) { return null }
const menu1 = <Menu><MenuItems /></Menu>; // OK
const menu2 = <Menu><BlueMenuItem /><MenuItem /></Menu>; // OK
const menu3 = <Menu><MenuItems /><MenuItem /><BlueMenuItem /></ Menu>; // OK
