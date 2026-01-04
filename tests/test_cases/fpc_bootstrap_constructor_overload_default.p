program fpc_bootstrap_constructor_overload_default;

{$mode objfpc}

type
  TMy = class
    constructor Create; overload;
    constructor Create(a: Integer; b: Integer = 5); overload;
  end;

constructor TMy.Create;
begin
  writeln('default');
end;

constructor TMy.Create(a: Integer; b: Integer);
begin
  writeln('args=', a, ',', b);
end;

var
  o: TMy;

begin
  o := TMy.Create(1);
end.
