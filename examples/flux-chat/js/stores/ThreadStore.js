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

var ChatAppDispatcher = require('../dispatcher/ChatAppDispatcher');
var ChatConstants = require('../constants/ChatConstants');
var ChatMessageUtils = require('../utils/ChatMessageUtils');
var EventEmitter = require('events').EventEmitter;

type RawMessage = {
  id: string;
  threadID: string;
  threadName: string;
  authorName: string;
  timestamp: number;
  text: string;
};

type Message = {
  id: string;
  threadID: string;
  authorName: string;
  date: Date;
  text: string;
  isRead: boolean;
};

type Thread = {
  id: string;
  name: string;
  lastMessage: Message;
};

type ThreadMap = {
  [index: string]: Thread;
};

type Callback = () => void;

var ActionTypes = ChatConstants.ActionTypes;
var CHANGE_EVENT = 'change';

var _currentID = null;
var _threads: ThreadMap = {};

var ThreadStore = Object.assign({}, EventEmitter.prototype, {

  init: function(rawMessages: Array<RawMessage>) {
    rawMessages.forEach(function(message) {
      var threadID = message.threadID;
      var thread = _threads[threadID];
      if (thread && thread.lastMessage.date.getTime() > message.timestamp) {
        return;
      }
      _threads[threadID] = {
        id: threadID,
        name: message.threadName,
        lastMessage: ChatMessageUtils.convertRawMessage(message, _currentID)
      };
    }, this);

    if (!_currentID) {
      var allChrono = this.getAllChrono();
      _currentID = allChrono[allChrono.length - 1].id;
    }

    _threads[_currentID].lastMessage.isRead = true;
  },

  emitChange: function() {
    this.emit(CHANGE_EVENT);
  },

  /**
   * @param {function} callback
   */
  addChangeListener: function(callback: Callback) {
    this.on(CHANGE_EVENT, callback);
  },

  /**
   * @param {function} callback
   */
  removeChangeListener: function(callback: Callback) {
    this.removeListener(CHANGE_EVENT, callback);
  },

  /**
   * @param {string} id
   */
  get: function(id: string): ?Thread {
    return _threads[id];
  },

  getAll: function(): ThreadMap {
    return _threads;
  },

  getAllChrono: function(): Array<Thread> {
    var orderedThreads = [];
    for (var id in _threads) {
      var thread = _threads[id];
      orderedThreads.push(thread);
    }
    orderedThreads.sort(function(a, b) {
      if (a.lastMessage.date < b.lastMessage.date) {
        return -1;
      } else if (a.lastMessage.date > b.lastMessage.date) {
        return 1;
      }
      return 0;
    });
    return orderedThreads;
  },

  getCurrentID: function(): ?string {
    return _currentID;
  },

  getCurrent: function(): ?Thread {
    var currentID = this.getCurrentID();
    if (currentID) {
      return this.get(currentID);
    } else {
      return null;
    }
  }

});

ThreadStore.dispatchToken = ChatAppDispatcher.register(function(payload: any) {
  var action = payload.action;

  switch(action.type) {

    case ActionTypes.CLICK_THREAD:
      _currentID = action.threadID;
      _threads[_currentID].lastMessage.isRead = true;
      ThreadStore.emitChange();
      break;

    case ActionTypes.RECEIVE_RAW_MESSAGES:
      ThreadStore.init(action.rawMessages);
      ThreadStore.emitChange();
      break;

    default:
      // do nothing
  }

});

module.exports = ThreadStore;
