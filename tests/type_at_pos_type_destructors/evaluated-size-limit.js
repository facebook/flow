// @flow

type Keys = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z';

// We should be printing "evaluated" section
type ObjSmall = {
//   ^
    p1: { [key in Keys]: string };
};

// We should not be printing "evaluated" section
type ObjBig = {
//   ^
    p1: { [key in Keys]: string };
    p2: { [key in Keys]: string };
    p3: { [key in Keys]: string };
    p4: { [key in Keys]: string };
};
