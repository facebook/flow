`main.js` declares a filesystem `Entry` hierarchy: a base `Entry`, plus
`FileEntry` (which has a `byteSize`) and `DirEntry` (which has a `children` list
of nested entries).

The `totalSize` function is already written: it sums the bytes of a file, or
recurses into a directory's children, by first asking the entry which kind it is.
Complete the declaration of `Entry` so that `totalSize` type-checks — when one of
those checks succeeds, the entry must be usable as the corresponding subtype
inside that branch.
