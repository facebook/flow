// @flow

import type {SUIButtonUniform} from './SUIButtonUniform';
import type {SUIActionMenuUniform} from './SUIActionMenuUniform';

const SUIExpressWiFiThemeComponents = require('./SUIExpressWiFiThemeComponents');

export type ComponentUniforms = {
  SUIActionMenu?: SUIActionMenuUniform,
  SUIButton?: SUIButtonUniform,
};

(SUIExpressWiFiThemeComponents: ComponentUniforms); // Error unannotated, so read/write optional properties fail to unify
