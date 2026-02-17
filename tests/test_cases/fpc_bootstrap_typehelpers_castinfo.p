{$mode objfpc}
{$modeswitch typehelpers}
program fpc_bootstrap_typehelpers_castinfo;

type
  TStrHelper = type helper for ShortString
    function AsciiSum: Integer;
  end;

function TStrHelper.AsciiSum: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Self) do
    Result := Result + Ord(Self[i]);
end;

type
  TA = class(TObject);
  TB = class(TA);

type
  THost = class
  public
    class function SeedSum(const S: ShortString): Integer; static;
  end;

class function THost.SeedSum(const S: ShortString): Integer;
begin
  Result := S.AsciiSum + 17;
end;

var
  A: TA;
  B: TB;
  FromCls, ToCls: ShortString;
  total: Integer;
  labelText: ShortString;

procedure ConsumeCastInfo;
begin
  FromCls := A.ClassName;
  ToCls := B.ClassName;
  total := total + FromCls.AsciiSum + ToCls.AsciiSum;
end;

begin
  total := 0;
  labelText := 'CastInfo';
  total := total + THost.SeedSum(labelText);

  A := TA.Create;
  B := TB.Create;
  FromCls := A.ClassName;
  ToCls := B.ClassName;
  writeln('start=', Ord(FromCls <> ''), ':', Ord(ToCls <> ''));
  writeln('cast_ok=', Ord(B <> nil));
  ConsumeCastInfo;
  A.Free;
  B.Free;
  writeln('total=', total);
end.
