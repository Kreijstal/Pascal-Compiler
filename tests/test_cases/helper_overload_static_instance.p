program helper_overload_static_instance;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TFoo = record
    Value: Integer;
  end;

  TFooHelper = type helper for TFoo
    function ToString(const Prefix: string): string; overload;
    class function ToString(const Value: TFoo): string; overload; static;
  end;

function TFooHelper.ToString(const Prefix: string): string;
begin
  if Self.Value = 7 then
    Result := Prefix + 'ok'
  else
    Result := 'bad';
end;

class function TFooHelper.ToString(const Value: TFoo): string;
begin
  if Value.Value = 7 then
    Result := 'ok'
  else
    Result := 'bad';
end;

var
  f: TFoo;

begin
  f.Value := 7;
  writeln(f.ToString('v='));
end.
