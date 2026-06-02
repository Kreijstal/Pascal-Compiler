program tdd_abstract_virtual_method_dispatch;
{$mode objfpc}{$H+}

{ Mirrors FPC symtype/symdef: an abstract virtual method in a base class,
  overridden in subclasses, called through a base-class reference.  Must
  dispatch through the VMT, NOT emit a direct call to the bodyless abstract
  base symbol. }

type
  TBaseDef = class
    function size: longint; virtual; abstract;
    function alignment: shortint; virtual; abstract;
  end;

  TOrdDef = class(TBaseDef)
    function size: longint; override;
    function alignment: shortint; override;
  end;

  TFloatDef = class(TBaseDef)
    function size: longint; override;
    function alignment: shortint; override;
  end;

function TOrdDef.size: longint; begin size := 4; end;
function TOrdDef.alignment: shortint; begin alignment := 1; end;
function TFloatDef.size: longint; begin size := 8; end;
function TFloatDef.alignment: shortint; begin alignment := 2; end;

procedure Report(d: TBaseDef);   { d is base-typed: call must be virtual }
begin
  writeln(d.size, ' ', d.alignment);
end;

var
  o, f: TBaseDef;
begin
  o := TOrdDef.Create;
  f := TFloatDef.Create;
  Report(o);
  Report(f);
  o.Free;
  f.Free;
end.
