{ Test TSystemCodePage type - standard FPC type for code pages }
{ TSystemCodePage is defined as Word in FPC RTL }
program tsystemcodepage_type;

var
  cp: TSystemCodePage;
begin
  cp := 65001;  { UTF-8 code page }
  WriteLn('Code page: ', cp);
  
  cp := 1252;   { Windows-1252 }
  WriteLn('Code page: ', cp);
end.
