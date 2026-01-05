program fpc_bootstrap_class_property_getsystemencoding;

{$mode objfpc}

type
  TMy = class
  strict private
    class function GetSystemEncoding: Integer; static;
  public
    class function GetDefault: Integer; static;
    class property SystemEncoding: Integer read GetSystemEncoding;
  end;

class function TMy.GetSystemEncoding: Integer;
begin
  Result := 42;
end;

class function TMy.GetDefault: Integer;
begin
  Result := GetSystemEncoding;
end;

begin
  writeln('def=', TMy.GetDefault);
end.
