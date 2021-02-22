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

fs.readFile(0, {}, (_, data) => {
  (data : Buffer);
});

/* readFileSync */

(fs.readFileSync("file.exp") : Buffer);
(fs.readFileSync("file.exp") : string); // error

(fs.readFileSync("file.exp", "blah") : string);
(fs.readFileSync("file.exp", "blah") : Buffer); // error

(fs.readFileSync("file.exp", { encoding: "blah" }) : string);
(fs.readFileSync("file.exp", { encoding: "blah" }) : Buffer); // error

(fs.readFileSync("file.exp", {}) : Buffer);
(fs.readFileSync("file.exp", {}) : string); // error

/* write */

(fs.write(0, Buffer.alloc(0), 0, 0, 0, (err, bytesWritten, buffer) => {
  (err: ?ErrnoError);
  (bytesWritten: number);
  (buffer: Buffer);
}));

(fs.write(0, Buffer.alloc(0), 0, 0, (err, bytesWritten, buffer) => {
  (err: ?ErrnoError);
  (bytesWritten: number);
  (buffer: Buffer);
}));

(fs.write(0, Buffer.alloc(0), (err, bytesWritten, buffer) => {
  (err: ?ErrnoError);
  (bytesWritten: number);
  (buffer: Buffer);
}));

(fs.write(0, "test", 0, "utf8", (err, written, str) => {
  (err: ?ErrnoError);
  (written: number);
  (str: string);
}));

(fs.write(0, "test", 0, (err, written, str) => {
  (err: ?ErrnoError);
  (written: number);
  (str: string);
}));

(fs.write(0, "test", (err, written, str) => {
  (err: ?ErrnoError);
  (written: number);
  (str: string);
}));

/* open */

(fs.open("file.exp", "r", (err, fd) => {
  (err: ?ErrnoError);
  (fd: number);
}));

(fs.open("file.exp", "r", 0o666, (err, fd) => {
  (err: ?ErrnoError);
  (fd: number);
}));

/* readdir */

(fs.readdir("file.exp", (err, files) => {
  (err: ?ErrnoError);
  (files: Array<string>);
}): void);

(fs.readdir("file.exp", "utf8", (err, files) => {
  (err: ?ErrnoError);
  (files: Array<string>);
}): void);

(fs.readdir("file.exp", { encoding: "utf8" }, (err, files) => {
  (err: ?ErrnoError);
  (files: Array<string>);
}): void);

(fs.readdir("file.exp", { withFileTypes: true }, (err, files) => {
  (err: ?ErrnoError);
  (files: Array<fs.Dirent>);
}): void);

(fs.readdir("file.exp", { withFileTypes: false }, (err, files) => {
  (err: ?ErrnoError);
  (files: Array<string>);
}): void);

(fs.readdir("file.exp", { withFileTypes: true }, (err, files) => {
  (err: ?ErrnoError);
  (files: Array<string>); // error
}): void);

(fs.readdir("file.exp", { encoding: "utf8", withFileTypes: true }, (err, files) => {
  (err: ?ErrnoError);
  (files: Array<fs.Dirent>);
}): void);

(fs.readdir("file.exp", { encoding: "utf8", withFileTypes: false }, (err, files) => {
  (err: ?ErrnoError);
  (files: Array<string>);
}): void);

/* readdirSync */

(fs.readdirSync("file.exp"): Array<string>);

(fs.readdirSync("file.exp", "utf8"): Array<string>);

(fs.readdirSync("file.exp", { encoding: "utf8" }): Array<string>);

(fs.readdirSync("file.exp", { withFileTypes: true }): Array<fs.Dirent>);

(fs.readdirSync("file.exp", { withFileTypes: false }): Array<string>);

(fs.readdirSync("file.exp", { withFileTypes: true }): Array<string>); // error

(fs.readdirSync("file.exp", { encoding: "utf8", withFileTypes: true }): Array<fs.Dirent>);

(fs.readdirSync("file.exp", { encoding: "utf8", withFileTypes: false }): Array<string>);
