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

module.exports = {

  init: function() {
    localStorage.clear();
    localStorage.setItem('messages', JSON.stringify([
      {
        id: 'm_1',
        threadID: 't_1',
        threadName: 'Jing and Bill',
        authorName: 'Bill',
        text: 'Hey Jing, want to give a Flux talk at ForwardJS?',
        timestamp: Date.now() - 99999
      },
      {
        id: 'm_2',
        threadID: 't_1',
        threadName: 'Jing and Bill',
        authorName: 'Bill',
        text: 'Seems like a pretty cool conference.',
        timestamp: Date.now() - 89999
      },
      {
        id: 'm_3',
        threadID: 't_1',
        threadName: 'Jing and Bill',
        authorName: 'Jing',
        text: 'Sounds good.  Will they be serving dessert?',
        timestamp: Date.now() - 79999
      },
      {
        id: 'm_4',
        threadID: 't_2',
        threadName: 'Dave and Bill',
        authorName: 'Bill',
        text: 'Hey Dave, want to get a beer after the conference?',
        timestamp: Date.now() - 69999
      },
      {
        id: 'm_5',
        threadID: 't_2',
        threadName: 'Dave and Bill',
        authorName: 'Dave',
        text: 'Totally!  Meet you at the hotel bar.',
        timestamp: Date.now() - 59999
      },
      {
        id: 'm_6',
        threadID: 't_3',
        threadName: 'Functional Heads',
        authorName: 'Bill',
        text: 'Hey Brian, are you going to be talking about functional stuff?',
        timestamp: Date.now() - 49999
      },
      {
        id: 'm_7',
        threadID: 't_3',
        threadName: 'Bill and Brian',
        authorName: 'Brian',
        text: 'At ForwardJS?  Yeah, of course.  See you there!',
        timestamp: Date.now() - 39999
      }
    ]));
  }

};
