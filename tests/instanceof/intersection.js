class Target {
  target: string = '';
}

class TextArea extends Target {
  value: string = '';
}

interface Labeled {
  label: string;
}

declare const memberExtends: Target & TextArea;

if (memberExtends instanceof TextArea) {
  memberExtends.value as string;
  memberExtends.target as string;
  memberExtends as empty; // error
} else {
  memberExtends as empty;
}

declare const interfaceMember: Labeled & TextArea;

if (interfaceMember instanceof Target) {
  interfaceMember.label as string;
  interfaceMember.value as string;
  interfaceMember as empty; // error
} else {
  interfaceMember as empty;
}

declare const noMemberExtends: Target & Labeled;

if (noMemberExtends instanceof TextArea) {
  noMemberExtends.value as string;
  noMemberExtends as empty; // error
} else {
  noMemberExtends.target as string;
  noMemberExtends.label as string;
  noMemberExtends as empty; // error
}
