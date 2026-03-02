program tdd_heaptrc_typedaddress_pointer;

{$TYPEDADDRESS on}

type
  TExtra = record
    data: record
    end;
  end;
  PExtra = ^TExtra;
  TOuter = record
    extra: PExtra;
  end;

procedure FillInfo(p: Pointer);
begin
end;

var
  fill_extra_info_proc: procedure(p: Pointer);
  outer: TOuter;
  extra: TExtra;
  pouter: ^TOuter;

begin
  fill_extra_info_proc := @FillInfo;
  outer.extra := @extra;
  pouter := @outer;
  fill_extra_info_proc(@pouter^.extra^.data);
  writeln('ok');
end.
