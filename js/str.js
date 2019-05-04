// Partially from
//  https://github.com/ejgallego/jscoq/blob/2718f9caf31398704c2d84ff089e3f6f0321eada/coq-js/js_stub/str.js

//Provides: str_ll
function str_ll(s, args) { if (str_ll.log) joo_global_object.console.warn(s, args); }
str_ll.log = false;

//Provides: re_string_match
//Requires: str_ll, re_match
function re_string_match(re, s, pos) {
  // external re_string_match : regexp -> string -> int -> int array
  //str_ll('re_string_match', arguments);
  var res = re_match(re, s, pos, 0);
  return (res === 0) ? [0] : res;
}

//Provides: re_search_forward
//Requires: str_ll, re_search_forward_naive
function re_search_forward(re, s, pos) {
  // external re_search_forward: regexp -> string -> int -> int array
  //str_ll('re_search_forward', arguments);
  return re_search_forward_naive(re, s, pos);
}

//Provides: re_partial_match
//Requires: str_ll
// external re_partial_match: regexp -> string -> int -> int array
function re_partial_match() { 
  //str_ll('re_partial_match', arguments); 
  return [0]; }
//Provides: re_replacement_text
//Requires: str_ll
// external re_replacement_text: string -> int array -> string -> string
function re_replacement_text(r, a, s) { 
  //str_ll('re_replacement_text', arguments); 
  return s; }
//Provides: re_search_backward
//Requires: str_ll
// external re_search_backward: regexp -> string -> int -> int array
function re_search_backward() { 
  //str_ll('re_search_backward', arguments); 
  return [0]; }

//Provides: re_match
// Based on
// https://github.com/ocaml/ocaml/blob/4.07/otherlibs/str/strstubs.c

var re_match = function () {

  var opcodes = {
    CHAR: 0, CHARNORM: 1, STRING: 2, STRINGNORM: 3, CHARCLASS: 4,
    BOL: 5, EOL: 6, WORDBOUNDARY: 7,
    BEGGROUP: 8, ENDGROUP: 9, REFGROUP: 10,
    ACCEPT: 11,
    SIMPLEOPT: 12, SIMPLESTAR: 13, SIMPLEPLUS: 14,
    GOTO: 15, PUSHBACK: 16, SETMARK: 17,
    CHECKPROGRESS: 18
  };

  function in_bitset(s, i) {
    return (s.c.charCodeAt(i >> 3) >> (i & 7)) & 1;
  }

  function re_match_impl(re, s, pos, partial) {

    var prog = re[1].slice(1),
      cpool = re[2].slice(1),
      numgroups = re[4],
      numregisters = re[5],
      startchars = re[6];

    var pc = 0, quit = false, txt = s.c,
      stack = [],
      groups = new Array(numgroups).fill(0).map(function () { return {}; }),
      re_register = new Array(numregisters);

    groups[0].start = pos;

    var backtrack = function () {
      while (stack.length) {
        var item = stack.pop(), obj, prop;
        if (item.undo) {
          [obj, prop] = item.undo.loc;
          obj[prop] = item.undo.value;
        }
        else {
          [pc, pos] = [item.pos.pc, item.pos.txt];
          return;
        }
      }
      quit = true;
    };
    var push = function (item) { stack.push(item); };


    var accept = function () {
      return Array.prototype.concat.apply([0],
        groups.map(function (g) { return g.start >= 0 && g.end >= 0 ? [g.start, g.end] : [-1, -1]; }))
    };

    /* Main DFA interpreter loop */
    while (!quit) {
      var op = prog[pc] & 0xff,
        sarg = prog[pc] >> 8,
        uarg = sarg & 0xff,
        c = txt.charCodeAt(pos),
        group;

      pc++;

      switch (op) {
        case opcodes.CHAR:
        case opcodes.CHARNORM:
          if (c === uarg) pos++;
          else backtrack();
          break;
        case opcodes.STRING:
        case opcodes.STRINGNORM:
          for (var w = cpool[uarg].c, i = 0; i < w.length; i++) {
            if (c === w.charCodeAt(i))
              c = txt.charCodeAt(++pos);
            else { backtrack(); break; }
          }
          break;
        case opcodes.CHARCLASS:
          if (!isNaN(c) && in_bitset(cpool[uarg], c)) pos++;
          else backtrack();
          break;

        case opcodes.BOL:
          if (pos > c.t && !isNaN(c) && txt.charCodeAt(pos - 1) !== '\n')
            backtrack()
          break

        case opcodes.EOL:
          if (pos < c.l && !isNaN(c) && c !== '\n')
            backtrack()
          break

        case opcodes.BEGGROUP:
          group = groups[uarg];
          push({
            undo: {
              loc: [group, 'start'],
              value: group.start
            }
          });
          group.start = pos;
          break;
        case opcodes.ENDGROUP:
          group = groups[uarg];
          push({
            undo: {
              loc: [group, 'end'],
              value: group.end
            }
          });
          group.end = pos;
          break;

        case opcodes.SIMPLEOPT:
          if (!isNaN(c) && in_bitset(cpool[uarg], c)) pos++;
          break;
        case opcodes.SIMPLESTAR:
          while (!isNaN(c) && in_bitset(cpool[uarg], c))
            c = txt.charCodeAt(++pos);
          break;
        case opcodes.SIMPLEPLUS:
          if (!isNaN(c) && in_bitset(cpool[uarg], c)) {
            do {
              c = txt.charCodeAt(++pos);
            } while (!isNaN(c) && in_bitset(cpool[uarg], c));
          }
          else backtrack();
          break;

        case opcodes.ACCEPT:
          groups[0].end = pos;
          return accept();

        case opcodes.GOTO:
          pc = pc + sarg;
          break;
        case opcodes.PUSHBACK:
          push({ pos: { pc: pc + sarg, txt: pos } });
          break;
        case opcodes.SETMARK:
          push({ undo: { loc: [re_register, uarg], value: re_register[uarg] } });
          re_register[uarg] = pos;
          break;
        case opcodes.CHECKPROGRESS:
          if (re_register[uarg] === pos) backtrack();
          break;

        default:
          throw new Error("unimplemented regexp opcode " + op + "(" + sarg + ")");
      }
    }

    return 0;
  }

  return re_match_impl;
}();


//Provides: re_search_forward_naive
//Requires: re_match
function re_search_forward_naive(re, s, pos) {
  while (pos < s.l) {
    var res = re_match(re, s, pos);
    if (res) return res;
    pos++;
  }

  return [0];  /* [||] : int array */
}
