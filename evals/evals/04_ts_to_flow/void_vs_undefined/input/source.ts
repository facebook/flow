type User = { id: number; name: string };

function findUser(users: User[], id: number): User | undefined {
  return users.find(u => u.id === id);
}

function greet(user: User | undefined): string {
  if (user === undefined) {
    return "Guest";
  }
  return `Hello, ${user.name}`;
}

const users: User[] = [
  { id: 1, name: "Ada" },
  { id: 2, name: "Bo" },
];

console.log(greet(findUser(users, 2)));
console.log(greet(findUser(users, 9)));
