declare class T {}
declare class U {}

declare var a: {...{ p :T },...{ p :U }}; a as { p:U };
declare var b: {...{ p?:T },...{ p :U }}; b as { p:U };
declare var c: {...{ p :T },...{ p?:U }}; c as { p:T|U };
declare var d: {...{ p?:T },...{ p?:U }}; d as { p?:T|U };

declare var e: {...{|p :T|},...{ p :U }}; e as { p :U };
declare var f: {...{|p?:T|},...{ p :U }}; f as { p:U };
declare var g: {...{|p :T|},...{ p?:U }}; g as { p :T|U };
declare var h: {...{|p?:T|},...{ p?:U }}; h as { p?:T|U };

declare var i: {...{ p :T },...{|p :U|}}; i as { p :  U };
declare var j: {...{ p?:T },...{|p :U|}}; j as { p :  U };
declare var k: {...{ p :T },...{|p?:U|}}; k as { p:T|U };
declare var l: {...{ p?:T },...{|p?:U|}}; l as { p?:T|U };

declare var m: {|...{|p :T|},...{|p :U|}|}; m as {|p :  U|};
declare var n: {|...{|p?:T|},...{|p :U|}|}; n as {|p :  U|};
declare var o: {|...{|p :T|},...{|p?:U|}|}; o as {|p :T|U|};
declare var p: {|...{|p?:T|},...{|p?:U|}|}; p as {|p?:T|U|};
