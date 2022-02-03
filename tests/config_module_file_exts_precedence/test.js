// @flow

// .zzz comes before .aaa
import {aaa_vs_zzz__aaa} from './aaa_vs_zzz'; // error
import {aaa_vs_zzz__zzz} from './aaa_vs_zzz';

// .aaa comes before .bbb
import {aaa_vs_bbb__aaa} from './aaa_vs_bbb';
import {aaa_vs_bbb__bbb} from './aaa_vs_bbb'; // error

// .aaa comes before .aaa.js even though .js comes before .aaa, because exact match wins
import {aaa_vs_aaa_js__aaa} from './aaa_vs_aaa_js.aaa';
import {aaa_vs_aaa_js__aaa__js} from './aaa_vs_aaa_js.aaa'; // error

// .zzz comes before .zzz.js
import {zzz_vs_zzz_js__zzz} from './zzz_vs_zzz_js.zzz';
import {zzz_vs_zzz_js__zzz__js} from './zzz_vs_zzz_js.zzz'; // error

// .js comes before .zzz.js even though .zzz comes before .js (resolved right to left)
import {js_vs_zzz_js__js} from './js_vs_zzz_js';
import {js_vs_zzz_js__zzz__js} from './js_vs_zzz_js'; // error

import {js_vs_zzz_js__js as js2} from './js_vs_zzz_js.zzz'; // error
import {js_vs_zzz_js__zzz__js as zzz_js2} from './js_vs_zzz_js.zzz';

// only one extension is stripped
import {multiple_exts as multiple_exts1} from './multiple_exts'; // error
import {multiple_exts as multiple_exts2} from './multiple_exts.zzz';
