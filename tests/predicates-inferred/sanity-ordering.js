// @flow

declare var key: string;
declare var obj: { page: ?Object; };

if (dotAccess(obj)) {
  (obj.page: Object);
}

function dotAccess(head, create) {
  const path = 'path.location';
  const stack = path.split('.');
  do {
    const key = stack.shift();
    if (!key) continue
    head = head[key] || create && (head[key] = {});
  } while (stack.length && head);
  return head;
}
