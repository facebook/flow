
import * as React from 'react';
import Tab from './Tab.react';
import TabGroup from './TabGroup.react';

declare function setSelectedTab(x: 'a'|'b'): void;
declare var selectedTab: 'a'|'b';

component Main() {
  return <TabGroup onChange={setSelectedTab /* TODO okay */} value={selectedTab}>
    <Tab value="a" />
  </TabGroup>;
}
