{$mode objfpc}
unit multi_unit_same_typed_const_name_unit_a;

interface

type
  pmymap = ^tmymap;
  tmymap = record
    name : string[20];
    cp   : word;
    next : pmymap;
  end;

var
  mappings : pmymap;

procedure registermymap(p : pmymap);

implementation

procedure registermymap(p : pmymap);
begin
  p^.next := mappings;
  mappings := p;
end;

const
  mymap : tmymap = (name : 'one'; cp : 1; next : nil);

begin
  registermymap(@mymap);
end.
