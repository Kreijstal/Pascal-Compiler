program derived_inherited_ctor_assign;

{$mode objfpc}

type
  TBase = class
    constructor Create(const Msg: string);
  end;

  TDerived = class(TBase)
  end;

constructor TBase.Create(const Msg: string);
begin
end;

var
  D: TDerived;
begin
  D := TDerived.Create('hi');
  writeln('ok');
end.
