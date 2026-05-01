// unwrapping nested maybes should work
'foo' as ?(?string) as ?string; // ok
123 as ?(?number) as ?string; // error (only num ~> string)
