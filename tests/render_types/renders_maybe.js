//@flow
import * as React from 'react';

component MenuItem() { return null }
component BlueMenuItem() renders MenuItem { return <MenuItem /> }
component MaybeMenuItem() renders renders? MenuItem { return null }
component Bar() {return true}
component Baz() renders null { return null }

(<MenuItem />: renders? MenuItem); // OK
(<BlueMenuItem />: renders? MenuItem); // OK
(<MaybeMenuItem />: renders? MenuItem); // OK
(<Bar />: renders? MenuItem); // ERROR
(<Baz />: renders? MenuItem); // OK

declare const AllMenuItems: renders? (MenuItem | BlueMenuItem | MaybeMenuItem);
(AllMenuItems: renders? MenuItem); // OK

declare const Bad: renders? (MenuItem | BlueMenuItem | MaybeMenuItem | Bar)
(Bad: renders? MenuItem); // ERROR

declare const rendersMenuItem: renders MenuItem; 
(rendersMenuItem: renders (true | renders? MenuItem));


declare const rendersMaybeMenuItem: renders? MenuItem;
(rendersMaybeMenuItem: renders MenuItem); // ERROR


declare const rendersMaybeUnion: renders? (MenuItem | Bar);
(rendersMaybeUnion: renders MenuItem); // ERROR

component RendersMaybeMenuItem() renders MaybeMenuItem { return <MaybeMenuItem /> }
(<RendersMaybeMenuItem />: renders? MenuItem); // OK

(rendersMaybeMenuItem: renders (null | false | void | MenuItem)); // OK

(rendersMaybeMenuItem: renders (null | false | void | Bar)); // ERROR

component MightRenderMenuItem() renders? MenuItem {
  return (null: null | MenuItem); // OK
}
