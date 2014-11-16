/**
 * @providesModule ErrorSignal
 * @flow
 * @nopackage
 */

var AsyncSignal = require('AsyncSignal');
var ErrorSignalConfig = require('ErrorSignalConfig');
var ScriptPath = require('ScriptPath');
var SiteData = require('SiteData');

function sendErrorSignal(category: string, data: object): void {
  // TODO: will need to use something other than the ErrorSignalConfig module
    new AsyncSignal(ErrorSignalConfig.uri, {c: category, m: data}).send();
}
/*
 * @param {string} category
 * @param {object} msg
 */
function logJSError(category: string, msg: object): void {
    msg.svn_rev = SiteData.revision;
    msg.script_path = ScriptPath.getScriptPath();
    sendErrorSignal(
        'javascript_error',
        JSON.stringify({c: category, m: msg})
    );
}

var ErrorSignal = {
    sendErrorSignal: sendErrorSignal,
    logJSError: logJSError
};

module.exports = ErrorSignal;
