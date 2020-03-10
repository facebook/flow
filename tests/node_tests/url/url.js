const url = require('url');
url.format(url.parse('https://example.com/foo'));

(url.parse('http://example.com/?foo=bar').query: empty); // error, string | null
(url.parse('http://example.com/?foo=bar', true).query: empty); // error, object

function f(parseQueryString: boolean) {
  (url.parse('http://example.com/?foo=bar', parseQueryString).query: empty); // error, string | null | object
}
