/**
 * This file is provided by Facebook for testing and evaluation purposes
 * only. Facebook reserves all rights not expressly granted.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * FACEBOOK BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * @flow
 */

var ChatConstants = require('../constants/ChatConstants');
var Dispatcher = require('flux').Dispatcher;

type RawMessage = {
  id: string;
  threadID: string;
  authorName: string;
  timestamp: number;
  text: string;
};

type ServerReceiveRawMessagesAction = {
  type: any;
  rawMessages: Array<RawMessage>;
};

type ServerReceiveRawCreatedMessageAction = {
  type: any;
  rawMessage: RawMessage;
};

type ServerAction = ServerReceiveRawMessagesAction
                  | ServerReceiveRawCreatedMessageAction;

type ViewCreateMessageAction = {
  type: any;
  text: string;
};

type ViewClickThreadAction = {
  type: any;
  threadID: string;
};

type ViewAction = ViewCreateMessageAction | ViewClickThreadAction;

type PayloadType = {
  source: any;
  action: ViewCreateMessageAction
        | ViewClickThreadAction
        | ServerReceiveRawMessagesAction
        | ServerReceiveRawCreatedMessageAction;
};

var PayloadSources = ChatConstants.PayloadSources;

var _dispatcherInstance: Dispatcher<PayloadType> = new Dispatcher();

var ChatAppDispatcher = {

  // WORKAROUND: flow prevents extension of class instances on run-time. Thus,
  // we cannot use Object.assign to extend a Dispatcher instance. Instead, we
  // work around this by simply wrapping all Dispatcher methods on our own.
  register: _dispatcherInstance.register.bind(_dispatcherInstance),
  unregister: _dispatcherInstance.unregister.bind(_dispatcherInstance),
  waitFor: _dispatcherInstance.waitFor.bind(_dispatcherInstance),
  dispatch: _dispatcherInstance.dispatch.bind(_dispatcherInstance),
  isDispatching: _dispatcherInstance.isDispatching.bind(_dispatcherInstance),

  /**
   * @param {object} action The details of the action, including the action's
   * type and additional data coming from the server.
   */
  handleServerAction: function(action: ServerAction) {
    var payload = {
      source: PayloadSources.SERVER_ACTION,
      action: action
    };
    this.dispatch(payload);
  },

  /**
   * @param {object} action The details of the action, including the action's
   * type and additional data coming from the view.
   */
  handleViewAction: function(action: ViewAction) {
    var payload = {
      source: PayloadSources.VIEW_ACTION,
      action: action
    };
    this.dispatch(payload);
  }

};

module.exports = ChatAppDispatcher;
