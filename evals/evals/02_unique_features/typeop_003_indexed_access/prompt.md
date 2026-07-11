Given the `ApiResponse` type, write a function `renderProfile(profile): string` that takes just the nested user-profile object and returns a display string built from its fields (for example, `"Ada <https://...>"`).

Derive the parameter's type from `ApiResponse` rather than re-declaring the profile shape, so it stays in sync if the response shape changes.
