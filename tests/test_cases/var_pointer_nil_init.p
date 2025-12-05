program var_pointer_nil_init;
{$mode objfpc}

{ Test: var declarations with pointer types and nil initializers }
{ Required for FPC system.pp: var calculated_cmdline: PAnsiChar = nil; }

type
  PMyChar = ^Char;
  PInteger = ^Integer;

var
  my_char_ptr: PMyChar = nil;
  my_int_ptr: PInteger = nil;
  generic_ptr: Pointer = nil;

begin
  writeln('my_char_ptr is nil: ', my_char_ptr = nil);
  writeln('my_int_ptr is nil: ', my_int_ptr = nil);
  writeln('generic_ptr is nil: ', generic_ptr = nil);
  writeln('All pointers initialized to nil correctly');
end.
