// @flow

declare var key: string;
declare var obj: { page: ?Object; };

if (dotAccess(obj, 'path.location')) {
  (obj.page: Object);
}

function dotAccess(head, path, create) {
  const stack = path.split('.');
  do {
    const key = stack.shift();
    head = head[key] || create && (head[key] = {});
  } while (stack.length && head);
  return head;
}
