{ Demonstrates that KGPC cannot evaluate set constants (set of char / ranges) }
{ Derived from FPC's system.pp bootstrap requirements (AllowDirectorySeparators) }
program FpcSetConstantChar;

const
  AllowDirectorySeparators: set of char = ['\', '/'];
  AllowDriveSeparators: set of char = [];
  AlphaSet: set of char = ['A' .. 'C'];

procedure Check(ch: char);
begin
  writeln(ch, ':',
          (ch in AllowDirectorySeparators), ':',
          (ch in AllowDriveSeparators), ':',
          (ch in AlphaSet));
end;

begin
  Check('\');
  Check('/');
  Check('A');
  Check('Z');
end.
