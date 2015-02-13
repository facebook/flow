#!/usr/bin/env node
var fs = require('fs');
var http = require('http');
//var data = fs.readFileSync('UnicodeData.txt');

/**
 * This script was another attempt to process unicode class data. It downloads
 * the unicode data, groups the code points by their class, and then calculates
 * the ranges for each class. Basically it's trying to find some concise way to
 * store the information about which code is which class
 */

function process_data(data) {
  console.log("Processing...");
  data = data.toString().split("\n").map(function(x) { return x.split(";"); });
  var classes = {};
  // Group codes by class
  for (var i = 0; i < data.length; i++) {
    if (data[i].length < 3) {
        continue;
    }
    var c = classes[data[i][2]] || [];
    c.push(data[i][0]);
    classes[data[i][2]] = c;
  }

  for (prop in classes) {
    console.log(prop, classes[prop].length);
  }

  var class_ranges = {}

  for (prop in classes) {
    var codes = classes[prop].map(function(x) { return parseInt(x, 16); });
    var ranges = [];
    var range = { start: codes[0], stop: codes[0] };
    for (var i = 1; i < codes.length; i++) {
      var c = codes[i];
      if (c == range.stop + 1) {
        range.stop = c;
      } else {
        ranges.push(range);
        range = { start: c, stop: c };
      }
    }
    ranges.push(range);
    class_ranges[prop] = ranges;
  }

  var results = []
  for (prop in class_ranges) {
    var result_for_class = [];
    var ranges = class_ranges[prop];
    for (var i = 0; i < ranges.length; i++) {
      result_for_class.push("(" + ranges[i].start + "," + ranges[i].stop + ")");
    }
    results.push('("' + prop + '",[' + result_for_class.join(",") + "])")
  }
  console.log("[" + results.join(",") + "]");
}

function handle_result(res) {
  var data = [];
  res.on("data", function (chunk) { data.push(chunk); });
  res.on("end", function () { process_data(data.join("")); });
}

var url = "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt";
console.log("Downloading %s", url);
// This allows this script to work on the devservers with the proxy
http.get(
  {
    host: "fwdproxy.any.facebook.com",
    port: 8080,
    path: "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt",
    headers: {
      Host: "www.unicode.org"
    }
  },
  handle_result
);
