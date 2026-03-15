{$mode delphi}
{$modeswitch advancedrecords}
program tdd_record_method_no_param_return_type;

type
  TFloatSpecial = (fsZero, fsInf, fsNInf);
  TRec = packed record
    function Exponent: LongInt;
    function Fraction: Extended;
    function SpecialType: TFloatSpecial;
  end;

function TRec.Exponent: LongInt;
begin
  Result := 42;
end;

function TRec.Fraction: Extended;
begin
  Result := 1.5;
end;

function TRec.SpecialType: TFloatSpecial;
begin
  Result := fsInf;
end;

function GetExp(const r: TRec): LongInt;
begin
  Result := TRec(r).Exponent;
end;

function IsInf(const r: TRec): Boolean;
begin
  Result := TRec(r).SpecialType in [fsInf, fsNInf];
end;

var
  r: TRec;
begin
  WriteLn(GetExp(r));
  WriteLn(Ord(IsInf(r)));
end.
