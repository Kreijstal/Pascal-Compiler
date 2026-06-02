program tdd_virtual_overload_dispatch;
{$mode objfpc}{$H+}

{ Regression: an overloaded method set that mixes a virtual and a non-virtual
  member must still dispatch by ARGUMENT SIGNATURE, not blindly through the
  virtual overload's VMT slot.

  This mirrors FPC's compiler ogbase.pas TExeOutput, which declares

      procedure MemPos_ExeSection(exesec: TExeSection);          { non-virtual }
      procedure MemPos_ExeSection(const aname: string); virtual;

  and, inside the string overload, calls MemPos_ExeSection(CurrExeSec) with a
  TExeSection argument -- which must reach the non-virtual class overload.

  KGPC used to mark every call to such a name as a virtual call and route it
  through the virtual sibling's VMT slot (matching siblings by name +
  parameter COUNT only, which cannot tell apart equal-arity overloads). The
  class-typed call then re-entered the string overload with the object pointer
  reinterpreted as a string. In FPC's self-host that left every executable
  section unpositioned (MemPos = Size = DataPos = 0) and raised internal error
  2012103002 in TExeOutput.WriteExeSectionContent.

  A depth guard keeps the buggy path finite: correct dispatch prints "SC"
  (string overload, then the class overload); the old miscompile re-enters the
  string overload and prints "SSSS+". }

type
  TSec = class
    name: string;
  end;

  TOut = class
    log: string;
    depth: integer;
    procedure Pos_Sec(sec: TSec); overload;
    procedure Pos_Sec(const aname: string); overload; virtual;
  end;

procedure TOut.Pos_Sec(sec: TSec);
begin
  log := log + 'C';
end;

procedure TOut.Pos_Sec(const aname: string);
var
  s: TSec;
begin
  Inc(depth);
  if depth > 4 then
  begin
    log := log + '+';
    Exit;
  end;
  log := log + 'S';
  s := TSec.Create;
  s.name := aname;
  Pos_Sec(s);            { TSec argument: must reach the non-virtual overload }
  s.Free;
end;

var
  o: TOut;
begin
  o := TOut.Create;
  o.log := '';
  o.depth := 0;
  o.Pos_Sec('.text');
  writeln(o.log);
  o.Free;
end.
