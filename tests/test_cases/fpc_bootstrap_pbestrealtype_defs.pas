unit fpc_bootstrap_pbestrealtype_defs;

{$mode objfpc}

interface

type
  TDef = class
    name: ShortString;
    constructor Create(const n: ShortString);
  end;

  TFloatDef = class(TDef)
  end;

  PDef = ^TDef;

  TBaseNode = class
    resultdef: TDef;
  end;

var
  bestrealtype: TDef;

const
  pbestrealtype: PDef = @bestrealtype;

implementation

constructor TDef.Create(const n: ShortString);
begin
  name := n;
end;

end.
