require('./all_platform_forked').foo as empty; // should see number ~> empty error since it imports the .js.flow file
require('./web_native_forked').foo as empty; // should see number ~> empty error since it imports the .js.flow file
require('../web/explicit_web.web'); // can resolve, but platform support error
require('../web/explicit_web'); // cannot resolve
require('../web/implicit_web'); // can resolve, but platform support error
require('../native/explicit_native.native'); // can resolve, but platform support error
require('../native/explicit_native'); // cannot resolve
require('../native/implicit_native'); // can resolve, but platform support error
