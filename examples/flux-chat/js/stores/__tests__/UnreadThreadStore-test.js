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
 */

jest.dontMock('../UnreadThreadStore');
jest.dontMock('object-assign');

describe('UnreadThreadStore', function() {

  var ChatAppDispatcher;
  var UnreadThreadStore;
  var callback;

  beforeEach(function() {
    ChatAppDispatcher = require('../../dispatcher/ChatAppDispatcher');
    UnreadThreadStore = require('../UnreadThreadStore');
    callback = ChatAppDispatcher.register.mock.calls[0][0];
  });

  it('registers a callback with the dispatcher', function() {
    expect(ChatAppDispatcher.register.mock.calls.length).toBe(1);
  });

  it('provides the unread thread count', function() {
    var ThreadStore = require('../ThreadStore');
    ThreadStore.getAll.mockReturnValueOnce(
      {
        foo: {lastMessage: {isRead: false}},
        bar: {lastMessage: {isRead: false}},
        baz: {lastMessage: {isRead: true}}
      }
    );
    expect(UnreadThreadStore.getCount()).toBe(2);
  });

});
