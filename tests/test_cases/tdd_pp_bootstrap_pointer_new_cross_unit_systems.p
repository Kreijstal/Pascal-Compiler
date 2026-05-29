unit tdd_pp_bootstrap_pointer_new_cross_unit_systems;
{ The "compiler/systems"-style unit: declares its OWN tinfo record
  (large) and a pointer alias pinfo = ^tinfo.  Also uses the other
  unit which declares a different, smaller record also named tinfo.
  The RegisterIt procedure is the FPC compiler's registertarget
  pattern in miniature: new(arr[t]) followed by arr[t]^ := r.  Under
  the bug, both kgpc_new and the trailing kgpc_move use the smaller
  record's size and the writeln below reads past the allocation. }
{$mode objfpc}
interface

uses tdd_pp_bootstrap_pointer_new_cross_unit_other;

type
  { Forward pointer declared BEFORE the matching record — mirrors
    compiler/systems.pas's `psysteminfo = ^tsysteminfo;` appearing on
    the line immediately above the tsysteminfo declaration.  At the
    point the forward pointer is processed, the only `tinfo` visible
    in scope is the one imported from the "other" unit, so any
    name-based resolution (FindSymbol on pointer_subtype_id) latches
    onto that 8-byte record instead of the 168-byte one declared
    below. }
  pinfo = ^tinfo;
  tinfo = record
    sentinel_lo : LongInt;
    payload     : array[1..40] of LongInt;
    sentinel_hi : LongInt;
  end;
  trange = (r_a, r_b, r_c);

var
  arr : array[trange] of pinfo;

procedure RegisterIt(const r : tinfo);
procedure DumpIt(t : trange);

implementation

procedure RegisterIt(const r : tinfo);
begin
  new(arr[r_b]);
  arr[r_b]^ := r;
end;

procedure DumpIt(t : trange);
begin
  WriteLn('lo=', arr[t]^.sentinel_lo);
  WriteLn('mid=', arr[t]^.payload[40]);
  WriteLn('hi=', arr[t]^.sentinel_hi);
end;

end.
