/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

%token EOF
%token DOT HYPHEN PLUS
%token LT LTE GT GTE EQ
%token CARET
%token <string> ID
%token <string> NR

%start version comparator range
%type <Semver_version.t> version
%type <Semver_comparator.t> comparator
%type <Semver_range.t> range
%%
version:
  number part part prerelease build {
    Semver_version.({
      major = $1;
      minor = $2;
      patch = $3;
      prerelease = $4;
      build = $5;
    })
  }
;

comparator:
  op version {
    Semver_comparator.({
      op = $1;
      version = $2;
    })
  }
;

number:
  NR { int_of_string $1 }

part:
/* empty */ { 0 }
| DOT number { $2 }
;

prerelease:
/* empty */ { [] }
| HYPHEN identifier_list { $2 }
;

build:
/* empty */ { [] }
| PLUS identifier_list { $2 }
;

identifier_list:
  identifier_part { [$1] }
| identifier_part DOT identifier_list { $1::$3 }
;

identifier_part:
  ID { Semver_version.Str $1 }
| NR { Semver_version.Int (int_of_string $1) }

op:
/* empty */ { None }
| LT { Some Semver_comparator.Less }
| LTE { Some Semver_comparator.LessOrEqual }
| GT { Some Semver_comparator.Greater }
| GTE { Some Semver_comparator.GreaterOrEqual }
| EQ { Some Semver_comparator.Equal }
;

/* RANGES */

range:
  range_part { [$1] }
| range_part range { $1::$2 }
;

range_part:
  comparator { Semver_range.Comparator $1 }
| CARET version { Semver_range.Caret $2 }
;
