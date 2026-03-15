program tdd_helper_class_method_static;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TStringHelper = type helper for string
    class function Fmt(const s: string): string; static;
    class function CallFmt: string; static;
    function Run: string;
  end;

class function TStringHelper.Fmt(const s: string): string;
begin
  Fmt := s;
end;

class function TStringHelper.CallFmt: string;
begin
  CallFmt := Fmt('ok');
end;

function TStringHelper.Run: string;
begin
  Run := CallFmt;
end;

var
  s: string;

begin
  s := 'x';
  Writeln(s.Run);
end.
