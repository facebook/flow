
import * as React from 'react';
import Tab from './Tab.react';
import TabGroup from './TabGroup.react';

declare function setSelectedTab(x: 'a'|'b'): void;
declare const selectedTab: 'a'|'b';

component Main() {
  return <TabGroup onChange={setSelectedTab} value={selectedTab}>
    <Tab value="a" />
  </TabGroup>;
}
