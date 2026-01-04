program fpc_bootstrap_constructor_overload_metaclass;

{$mode objfpc}

type
  TBase = class
    constructor Create; overload; virtual;
    constructor Create(a: Integer); overload; virtual;
  end;

  TChild = class(TBase)
  end;

  TMeta = class of TBase;

constructor TBase.Create;
begin
  writeln('default');
end;

constructor TBase.Create(a: Integer);
begin
  writeln('int=', a);
end;

var
  cls: TMeta;
  obj: TBase;

begin
  cls := TChild;
  obj := cls.Create(5);
end.
