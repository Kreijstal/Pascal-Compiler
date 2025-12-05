program widechar_type;
{ Test WideChar type support - required for FPC system.pp compatibility }

type
  TMyChar = WideChar;
  
var
  w: WideChar;
  m: TMyChar;

begin
  w := 'A';
  m := 'B';
  writeln('SizeOf(WideChar): ', SizeOf(WideChar));
  writeln('SizeOf(TMyChar): ', SizeOf(TMyChar));
end.
