import {foo} from './unsound_extract_exports';
foo(); // error: the imported method retains its receiver
