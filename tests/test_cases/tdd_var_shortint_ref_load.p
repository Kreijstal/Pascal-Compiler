program varshort_test;
{ Regression: reading a by-reference (var) parameter of a narrow signed type
  (shortint) must load exactly one sign-extended byte, not a 4-byte movl that
  picks up adjacent memory.  This mirrors FPC symtable.pas getfieldoffset's
  `var globalfieldalignment: shortint`, whose mis-sized read corrupted record
  packrecords-C alignment and over-sized every record to 16 bytes. }
type
  packed_state = record
    a: shortint;
    b: shortint;
    c: shortint;
    d: shortint;
  end;
var
  st: packed_state;

function read_via_ref(var v: shortint): longint;
begin
  read_via_ref := v;
end;

begin
  { Put distinctive nonzero bytes in every field so a 4-byte read of `a`
    would observe 0x04030201 worth of garbage rather than just 1. }
  st.a := 1;
  st.b := 2;
  st.c := 3;
  st.d := 4;
  writeln(read_via_ref(st.a));
  writeln(read_via_ref(st.b));
end.
