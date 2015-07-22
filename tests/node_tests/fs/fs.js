var fs = require("fs");

/* readFile */

fs.readFile("file.exp", (_, data) => {
  (data : Buffer);
});

fs.readFile("file.exp", "blah", (_, data) => {
  (data : string);
});

fs.readFile("file.exp", { encoding: "blah" }, (_, data) => {
  (data : string);
});

fs.readFile("file.exp", {}, (_, data) => {
  (data : Buffer);
});

/* readFileSync */

(fs.readFileSync("file.exp") : Buffer);

(fs.readFileSync("file.exp", "blah") : string);

(fs.readFileSync("file.exp", { encoding: "blah" }) : string);

(fs.readFileSync("file.exp", {}) : Buffer);
