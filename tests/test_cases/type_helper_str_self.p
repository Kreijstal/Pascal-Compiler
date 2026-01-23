{ Test: Str builtin with type helper Self }
{ BUG: Str(Self, Result) should work in type helper for Byte }
program type_helper_str_self;

type
  TByteHelper = type helper for Byte
    function ToString: string;
  end;

function TByteHelper.ToString: string;
begin
  Str(Self, Result);
end;

var
  B: Byte;
begin
  B := 42;
  WriteLn(B.ToString);
end.
