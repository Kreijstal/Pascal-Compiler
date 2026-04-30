program tdd_inherited_tobject_virtual_slot_override;
{$mode objfpc}

type
  TBase = class
    procedure Init; virtual;
    procedure Other; virtual;
  end;

  TMid = class(TBase)
    procedure Other; override;
  end;

  TGrand = class(TMid)
    procedure Init; override;
  end;

procedure TBase.Init;
begin
  WriteLn('base-init');
end;

procedure TBase.Other;
begin
  WriteLn('base-other');
end;

procedure TMid.Other;
begin
  WriteLn('mid-other');
end;

procedure TGrand.Init;
begin
  WriteLn('grand-init');
end;

var
  B: TBase;

begin
  B := TGrand.Create;
  B.Init;
  B.Other;
end.
