type UserId = string & { readonly __brand: "UserId" };
type PostId = string & { readonly __brand: "PostId" };

function makeUserId(raw: string): UserId {
  return raw as UserId;
}

function makePostId(raw: string): PostId {
  return raw as PostId;
}

interface Post {
  id: PostId;
  authorId: UserId;
  title: string;
}

function createPost(id: string, authorId: string, title: string): Post {
  return {
    id: makePostId(id),
    authorId: makeUserId(authorId),
    title,
  };
}

function authorOf(post: Post): UserId {
  return post.authorId;
}

function describe(userId: UserId, post: Post): string {
  return `${userId} wrote "${post.title}" (#${post.id})`;
}

const post = createPost("p1", "u1", "Hello, world");
const author = authorOf(post);

console.log(describe(author, post));
