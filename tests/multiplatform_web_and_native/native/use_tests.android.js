require('../xplat/all_platform_forked').foo as empty; // should see number ~> empty error since it imports the .js.flow file
require('../xplat/web_native_forked').foo as empty; // should see string ~> empty error since it imports the .native.js file
require('../web/explicit_web.web'); // can resolve, but platform support error
require('../web/explicit_web'); // cannot resolve
require('../web/implicit_web'); // can resolve, but platform support error
require('./explicit_native.native'); // can resolve
require('./explicit_native'); // can resolve
require('./implicit_native'); // can resolve
