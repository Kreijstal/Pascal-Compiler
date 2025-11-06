program OverrideSingleInheritance;

{ Unit test for override keyword with single inheritance chain
  Tests that override properly replaces the parent method in VMT }

type
  TLevel1 = class
    V1: Integer;
    procedure ShowLevel; virtual;
  end;
  
  TLevel2 = class(TLevel1)
    V2: Integer;
    procedure ShowLevel; override;
  end;
  
  TLevel3 = class(TLevel2)
    V3: Integer;
    procedure ShowLevel; override;
  end;

procedure TLevel1.ShowLevel;
begin
  WriteLn('Level 1');
end;

procedure TLevel2.ShowLevel;
begin
  WriteLn('Level 2');
end;

procedure TLevel3.ShowLevel;
begin
  WriteLn('Level 3');
end;

var
  L1: TLevel1;
  L2: TLevel2;
  L3: TLevel3;
begin
  WriteLn('Testing override in inheritance chain:');
  
  L1.V1 := 1;
  L1.ShowLevel;
  
  L2.V1 := 1;
  L2.V2 := 2;
  L2.ShowLevel;
  
  L3.V1 := 1;
  L3.V2 := 2;
  L3.V3 := 3;
  L3.ShowLevel;
end.
