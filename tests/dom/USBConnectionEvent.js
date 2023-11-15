// @flow


let tests = [
    // USBConnectionEvent
    function(document: Document) {
      const connect = new USBConnectionEvent('connect');
      const disconnect = new USBConnectionEvent('disconnect');
    }
  ];
