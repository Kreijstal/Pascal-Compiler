program tdd_hminus_virtual_string_return;
{$mode objfpc}
{$H-}

type
  TBase = class
    function Prefix: string; virtual; abstract;
    function Combined: string;
  end;

  TChild = class(TBase)
    function Prefix: string; override;
  end;

function TBase.Combined: string;
begin
  Result := Prefix;
  Result := Result + 'def';
end;

function TChild.Prefix: string;
begin
  Result := 'abc';
end;

var
  B: TBase;
begin
  B := TChild.Create;
  Writeln(B.Combined);
end.
