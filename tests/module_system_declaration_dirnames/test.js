import foo from 'foo';
foo as empty; // error: string ~> empty
import bar from 'bar'; // error: @flowtyped is not special when module.declaration_dirnames is explicitly set
import baz from 'baz';
baz as empty; // error: string ~> empty
