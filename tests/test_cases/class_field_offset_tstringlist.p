program class_field_offset_tstringlist;

{$mode objfpc}

uses
  Classes, SysUtils;

var
  sl: TStringList;

begin
  sl := TStringList.Create;
  try
    sl.Add('alpha');
    sl.Add('beta');
    sl.Add('gamma');
    Writeln('count=', sl.Count);
    Writeln('first=', sl[0]);
    Writeln('last=', sl[sl.Count - 1]);
  finally
    sl.Free;
  end;
end.
