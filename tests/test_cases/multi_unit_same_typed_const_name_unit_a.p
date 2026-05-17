{$mode objfpc}
unit multi_unit_same_typed_const_name_unit_a;

{ Helper for multi_unit_same_typed_const_name regression test.
  Declares a typed-const named `mapentry` and a global linked-list head
  named `mappings`, then registers @mapentry from its initialization
  block.  Unit B uses the same name `mapentry` for a DIFFERENT typed
  const, exercising the same-name-cross-unit collision bug fixed
  alongside the FPC `widestr.pas` charset-map crash. }

interface

type
  pmapentry = ^tmapentry;
  tmapentry = record
    name : string[20];
    code : word;
    next : pmapentry;
  end;

var
  mappings : pmapentry;

procedure registermap(p : pmapentry);

implementation

procedure registermap(p : pmapentry);
begin
  p^.next := mappings;
  mappings := p;
end;

const
  mapentry : tmapentry = (name : 'unit_a'; code : 11; next : nil);

begin
  registermap(@mapentry);
end.
