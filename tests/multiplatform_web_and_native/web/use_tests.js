require('../xplat/all_platform_forked').foo as empty; // should see boolean ~> empty error since it imports the .js.flow file
require('../xplat/web_native_forked').foo as empty; // should see boolean ~> empty error since it imports the .js.flow file
require('./explicit_web.web'); // can resolve
require('./explicit_web'); // can resolve
require('./implicit_web'); // can resolve
require('../native/explicit_native.native'); // can resolve, but platform support error
require('../native/explicit_native'); // cannot resolve
require('../native/implicit_native'); // can resolve, but platform support error
