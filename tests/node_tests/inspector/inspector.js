const inspector = require('inspector');

/* open */

inspector.open();
inspector.open(8080);
inspector.open(8080, 'http://localhost');
inspector.open(8080, 'http://localhost', true);

inspector.open('8080'); // error
inspector.open(8080, 127001); // error
inspector.open(8080, 'http://localhost', 1000); // error

(inspector.open() : void);

(inspector.open() : string); // error

/* close */

inspector.close();

(inspector.close() : void);

(inspector.close() : string); // error

/* console */

(inspector.console : Object);

/* url */

inspector.url();

(inspector.url() : string | void);

(inspector.url() : number); // error

/* waitForDebugger */

inspector.waitForDebugger();

(inspector.waitForDebugger() : void);

(inspector.waitForDebugger() : number); // error

/* Session.connect */

var session = new inspector.Session();

session.connect();

(session.connect() : void);

(inspector.connect() : number); // error

/* Session.events */

session.on('foo', () => {});

/* Session.connectToMainThread */

session = new inspector.Session();

session.connectToMainThread();

(session.connectToMainThread() : void);

(inspector.connectToMainThread() : number); // error

/* Session.disconnect */

session = new inspector.Session();

session.disconnect();

(session.disconnect() : void);

(inspector.disconnect() : number); // error

/* Session.post */

session.post('Profiler.enable');
session.post('Runtime.evaluate', { expression: '2 + 2' });
session.post('Profiler.enable', null, () => {});

session.post(); // error

(session.post('Profiler.enable') : void);

(session.post() : string); // error
