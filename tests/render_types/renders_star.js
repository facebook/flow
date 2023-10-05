import * as React from 'react';
component MenuItem() { return null }
(null: RendersStar<MenuItem>); // OK
(false: RendersStar<MenuItem>); // OK
(undefined: RendersStar<MenuItem>); // OK
([null, [false, [undefined]]]: RendersStar<MenuItem>); // OK
([null, false, undefined]: RendersStar<MenuItem>); // OK
(true: RendersStar<MenuItem>); // ERROR

(<MenuItem />: RendersStar<MenuItem>); // OK
declare const menuItemChildrenArray: React.ChildrenArray<MenuItem>;
(menuItemChildrenArray: RendersStar<MenuItem>); // OK

component BlueMenuItem() renders MenuItem { return <MenuItem />; }
(<BlueMenuItem />: RendersStar<MenuItem>); // OK

declare const rendersMaybeBlueMenuItem: RendersHuh<MenuItem>;
(rendersMaybeBlueMenuItem: RendersStar<MenuItem>); // OK

declare const rendersChildrenArrayBlueMenuItem: RendersHuh<React.ChildrenArray<BlueMenuItem>>;
(rendersChildrenArrayBlueMenuItem: RendersStar<MenuItem>); // OK

component Bad() { return null }
declare const rendersHuhBad: RendersHuh<Bad>;
(rendersHuhBad: RendersStar<MenuItem>); // ERROR

declare const rendersStarMenuItem: RendersStar<MenuItem>;
(rendersStarMenuItem: renders (null | false | void | MenuItem | $ReadOnlyArray<RendersStar<MenuItem>>)); // OK

([<MenuItem />, <BlueMenuItem />, <Bad />]: RendersStar<MenuItem>); // ERROR
