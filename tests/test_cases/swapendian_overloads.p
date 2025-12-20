program SwapEndianOverloads;

var
  a: LongInt;
  b: Int64;

begin
  a := SwapEndian(LongInt($11223344));
  b := SwapEndian(Int64($0102030405060708));
  if (a = LongInt($44332211)) and (b = Int64($0807060504030201)) then
    writeln('OK')
  else
    writeln('BAD');
end.
