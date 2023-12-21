component Comp() {} // error: unsupported

// But fragments are still using component syntax and are not any:
(3: React$FragmentType); // ERROR
module.exports = { Comp };
