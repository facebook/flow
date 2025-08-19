// Intentionally make the literal non-disjoint so the usual disjoint union optimization cannot kick in.
// This test represents the structure of ESTree, which we cannot change, that is not disjoint union, but it's very close of being one
type Literal1 = {type: 'Literal', subtype: 1};
type Literal2 = {type: 'Literal', subtype: 2};
type Expression00 = {type: 'Expression00', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression01 = {type: 'Expression01', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression02 = {type: 'Expression02', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression03 = {type: 'Expression03', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression04 = {type: 'Expression04', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression05 = {type: 'Expression05', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression06 = {type: 'Expression06', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression07 = {type: 'Expression07', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression08 = {type: 'Expression08', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression09 = {type: 'Expression09', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression10 = {type: 'Expression10', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression11 = {type: 'Expression11', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression12 = {type: 'Expression12', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression13 = {type: 'Expression13', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression14 = {type: 'Expression14', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression15 = {type: 'Expression15', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression16 = {type: 'Expression16', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression17 = {type: 'Expression17', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression18 = {type: 'Expression18', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression19 = {type: 'Expression19', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression20 = {type: 'Expression20', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression21 = {type: 'Expression21', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression22 = {type: 'Expression22', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression23 = {type: 'Expression23', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression24 = {type: 'Expression24', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression25 = {type: 'Expression25', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression26 = {type: 'Expression26', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression27 = {type: 'Expression27', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression28 = {type: 'Expression28', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression29 = {type: 'Expression29', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression30 = {type: 'Expression30', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression31 = {type: 'Expression31', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression32 = {type: 'Expression32', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression33 = {type: 'Expression33', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression34 = {type: 'Expression34', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression35 = {type: 'Expression35', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression36 = {type: 'Expression36', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression37 = {type: 'Expression37', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression38 = {type: 'Expression38', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression39 = {type: 'Expression39', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression40 = {type: 'Expression40', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression41 = {type: 'Expression41', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression42 = {type: 'Expression42', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression43 = {type: 'Expression43', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression44 = {type: 'Expression44', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression45 = {type: 'Expression45', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression46 = {type: 'Expression46', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression47 = {type: 'Expression47', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression48 = {type: 'Expression48', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Expression49 = {type: 'Expression49', aaa: {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Node}};
type Node =
  | Literal1 | Literal2
  | Expression00 | Expression01 | Expression02 | Expression03 | Expression04
  | Expression05 | Expression06 | Expression07 | Expression08 | Expression09
  | Expression10 | Expression11 | Expression12 | Expression13 | Expression14
  | Expression15 | Expression16 | Expression17 | Expression18 | Expression19
  | Expression20 | Expression21 | Expression22 | Expression23 | Expression24
  | Expression25 | Expression26 | Expression27 | Expression28 | Expression29
  | Expression30 | Expression31 | Expression32 | Expression33 | Expression34
  | Expression35 | Expression36 | Expression37 | Expression38 | Expression39
  | Expression40 | Expression41 | Expression42 | Expression43 | Expression44
  | Expression45 | Expression46 | Expression47 | Expression48 | Expression49;

type BigObject1 = {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Obj};
type BigObject2 = {foo:string,bar:number,baz:{beep:string,boop:number},zucc:boolean,rec:Obj};
type Obj =
  | {type: 'Expression48', aaa: BigObject1}
  | {type: 'Expression49', aaa: BigObject2}

declare const obj: Obj

// $FlowFixMe[incompatible-type]
obj as Node;
