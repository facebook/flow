require('./android_only'); // ok
require('./android_and_common').foo as string; // error: android_and_common.android is actually imported
