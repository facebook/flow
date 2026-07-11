Write Flow code for a permission system.

Define an enum `Permission` with members `Read`, `Write`, `Execute`, and `Admin`.

Write:
- `parsePermission(input: string): Permission` — convert a raw string to the enum type, returning `Permission.Read` as the default if the string is not valid
- `canModify(perm: Permission): boolean` — return `true` if the permission is `Write` or `Admin`, `false` otherwise
- `describeAccess(perm: Permission): string` — return a human-readable description: Read="view only", Write="can edit", Execute="can run", Admin="full control"
