//= require require_2_3_3
//= require flow-loader

// This is a plain .js file (no Babel), so stick to ES5

var versionCache = {};

this.onmessage = function(e) {
  var data = e.data;
  switch (data.type) {
    // preload flow. optional, but makes sure flow is ready
    case "init":
      getFlow(data.version).then(function() {
        postMessage({id: data.id, type: "init", result: true});
      })["catch"](function (e) { throw e; });
      return;
    case "checkContent":
      getFlow(data.version).then(function(flow) {
        var result = flow.checkContent(data.filename, data.body);
        console.log(result);
        postMessage({id: data.id, type: "checkContent", result: result });
      })["catch"](function (e) { throw e; });
      return;
    default:
      throw new Error("Unknown message type: " + data.type);
  }
};

function getFlow(version) {
  if (!(version in versionCache)) {
    versionCache[version] = new Promise(function(resolve) {
      require(['flow-loader'], function(FlowLoader) {
        resolve(FlowLoader.load(version));
      });
    });
  }
  return versionCache[version];
}
//
// this.console = {
//   log: function(v) { postMessage({type: "debug", message: v}); }
// };
