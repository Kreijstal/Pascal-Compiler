program runtime_features;

uses SysUtils;

type
  TNumbers = array of NativeUInt;

var
  nums: TNumbers;
  i: integer;
  total: NativeUInt;
  sample, sub: string;

begin
  SetLength(nums, 4);
  for i := 0 to 3 do
    nums[i] := i + 1;

  total := 0;
  for i := 0 to 3 do
    Inc(total, nums[i]);
  writeln(total);

  sample := 'PascalCompiler';
  sub := Copy(sample, 7, 8);
  writeln(sub);
  writeln(Length(sub));

  i := 0;
  while i < 10 do
  begin
    Inc(i);
    if i = 3 then
      break;
  end;
  writeln(i);
end.
