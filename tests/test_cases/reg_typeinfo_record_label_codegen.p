program reg_typeinfo_record_label_codegen;
{$mode objfpc}

type
  TMyRec = record
    Line: LongInt;
    Column: Word;
  end;

var
  P: Pointer;
begin
  P := TypeInfo(TMyRec);
  if P <> nil then
    WriteLn('ok');
end.
