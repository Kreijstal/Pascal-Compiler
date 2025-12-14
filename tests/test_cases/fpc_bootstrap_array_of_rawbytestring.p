{ Test: Open array of RawByteString as function parameter }
{ From FPC unixutil.pp - ArrayStringToPPchar function }
{ This pattern is required for FPC RTL bootstrap }
program fpc_bootstrap_array_of_rawbytestring;
{$mode objfpc}

function CountStrings(const S: Array of RawByteString): Integer;
begin
  Result := High(s) - Low(s) + 1;
end;

begin
  WriteLn(CountStrings(['A', 'B', 'C']));
end.
