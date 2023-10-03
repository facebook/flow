//@flow
import * as React from 'react';

component MenuItem() { return null }
component BlueMenuItem() renders MenuItem { return <MenuItem /> }
component MaybeMenuItem() renders RendersHuh<MenuItem> { return null }
component Bar() {return true}
component Baz() renders null { return null }

(<MenuItem />: RendersHuh<MenuItem>); // OK
(<BlueMenuItem />: RendersHuh<MenuItem>); // OK
(<MaybeMenuItem />: RendersHuh<MenuItem>); // OK
(<Bar />: RendersHuh<MenuItem>); // ERROR
(<Baz />: RendersHuh<MenuItem>); // OK

declare const AllMenuItems: RendersHuh<MenuItem | BlueMenuItem | MaybeMenuItem>
(AllMenuItems: RendersHuh<MenuItem>); // OK

declare const Bad: RendersHuh<MenuItem | BlueMenuItem | MaybeMenuItem | Bar>
(Bad: RendersHuh<MenuItem>); // ERROR

declare const rendersMenuItem: renders MenuItem; 
(rendersMenuItem: renders (true | RendersHuh<MenuItem>));


declare const rendersMaybeMenuItem: RendersHuh<MenuItem>;
(rendersMaybeMenuItem: renders MenuItem); // ERROR


declare const rendersMaybeUnion: RendersHuh<MenuItem | Bar>;
(rendersMaybeUnion: renders MenuItem); // ERROR

component RendersMaybeMenuItem() renders MaybeMenuItem { return <MaybeMenuItem /> }
(<RendersMaybeMenuItem />: RendersHuh<MenuItem>); // OK

(rendersMaybeMenuItem: renders (null | false | void | MenuItem)); // OK

(rendersMaybeMenuItem: renders (null | false | void | Bar)); // ERROR
