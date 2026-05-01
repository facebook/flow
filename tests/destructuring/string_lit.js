var { "key": val } = { key: "val" };
val as void; // error: string ~> void

var { "with-dash": with_dash } = { "with-dash": "motivating example" as const };
with_dash as "motivating example"; // ok
