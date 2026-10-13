// References inside each `declare namespace` block see the members merged across blocks.
const options: LibMergedMember.Options = {a: 1, b: 'x'}; // ok
const thing = new LibMergedMember.Thing(options); // ok
thing.get() as string; // ok
thing.extra() as number; // ok: `Thing` in the other block's constructor is the merged `Thing`
LibMergedMember.make() as LibMergedMember.Thing; // ok
'a' as LibMergedMember.Kind; // ok: `Options` in this block is the merged `Options`
'b' as LibMergedMember.Kind; // ok
'c' as LibMergedMember.Kind; // err: not a key of the merged `Options`

// A `declare class` merges with the interface of the same name in its own block.
declare const foo: LibMergedClass.Foo;
foo.a as number; // ok
foo.b as string; // ok
foo.c as boolean; // ok
new LibMergedClass.Foo().c as boolean; // ok
