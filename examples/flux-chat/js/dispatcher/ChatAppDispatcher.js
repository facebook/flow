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
  action: any/*ServerAction | ViewAction*/;
};

var PayloadSources = ChatConstants.PayloadSources;

var dispatcherInstance: Dispatcher<PayloadType> = new Dispatcher();

var ChatAppDispatcher = Object.assign({}, dispatcherInstance, {

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

});

module.exports = ChatAppDispatcher;
