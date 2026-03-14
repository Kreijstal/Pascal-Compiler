{ Test GetLocalTime and DecodeDate/DecodeTime }
program fpc_getlocaltime;
{$mode objfpc}
uses SysUtils;
var
  st: TSystemTime;
begin
  GetLocalTime(st);
  { Year should be >= 2024 }
  if st.Year >= 2024 then
    WriteLn('year ok')
  else
    WriteLn('year bad: ', st.Year);
  { Month should be 1-12 }
  if (st.Month >= 1) and (st.Month <= 12) then
    WriteLn('month ok')
  else
    WriteLn('month bad: ', st.Month);
end.
