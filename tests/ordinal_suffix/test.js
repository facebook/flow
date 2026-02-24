// Test that ordinal suffixes for the 11th, 12th, and 13th parameters
// are correctly rendered as "11th", "12th", "13th" (not "11st", "12nd", "13rd").

// Use a function type with unnamed parameters so that the error message
// shows "the Nth parameter" with the ordinal form.
declare const f: (
  number, number, number, number, number,
  number, number, number, number, number,
  number, number, number,
) => void;

f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, "eleven", "twelve", "thirteen");
