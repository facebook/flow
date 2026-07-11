Write a function `isEnabled(flags: FeatureFlags, name): boolean` that returns whether a given feature is turned on by looking it up in `flags`.

The `name` parameter must only accept the names of features that actually exist in `FeatureFlags` — passing any other string should be a type error. Derive the set of valid names from `FeatureFlags` itself so it stays correct when a feature is added or removed, rather than listing the names by hand.
