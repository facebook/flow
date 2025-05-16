import {foo} from 'test_lib_cjs'
//       ^
import {foo as fooo} from 'test_lib_cjs';
//       ^
import {foo as boo} from 'test_lib_cjs';
//              ^
import {bar} from'test_lib_cjs';
//       ^
import {bar as baaaar} from'test_lib_cjs';
//       ^
import {bar as far} from 'test_lib_cjs';
//              ^
import defaultImport from './helpers/module_exports_ref';
//              ^
