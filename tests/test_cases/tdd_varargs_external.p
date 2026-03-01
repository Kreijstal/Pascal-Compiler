program varargs_test;

uses ctypes;

function c_snprintf(buf: PChar; size: csize_t; fmt: PChar): cint; cdecl; varargs; external name 'snprintf';

var
  buf: array[0..255] of char;
  x: cint;
  d: double;
begin
  x := 42;
  d := 3.14;

  { Call with 2 extra args beyond formal params }
  c_snprintf(@buf[0], 256, '%d %d', x, cint(7));
  writeln(PChar(@buf[0]));

  { Call with 1 extra arg }
  c_snprintf(@buf[0], 256, '%.2f', d);
  writeln(PChar(@buf[0]));

  { Call with 0 extra args }
  c_snprintf(@buf[0], 256, 'no extras');
  writeln(PChar(@buf[0]));
end.
