program hresult_const_typecast;

{ Test HRESULT const typecast support - needed for FPC sysutils.pp }

const
  { Standard COM HRESULT values using HRESULT typecast }
  VAR_OK = HRESULT($00000000);
  VAR_PARAMNOTFOUND = HRESULT($80020004);
  VAR_TYPEMISMATCH = HRESULT($80020005);
  VAR_BADVARTYPE = HRESULT($80020008);
  VAR_EXCEPTION = HRESULT($80020009);
  VAR_OVERFLOW = HRESULT($8002000A);
  VAR_BADINDEX = HRESULT($8002000B);
  VAR_ARRAYISLOCKED = HRESULT($8002000D);
  VAR_NOTIMPL = HRESULT($80004001);
  VAR_OUTOFMEMORY = HRESULT($8007000E);
  VAR_INVALIDARG = HRESULT($80070057);
  VAR_UNEXPECTED = HRESULT($8000FFFF);

  { S_OK and S_FALSE are common HRESULT values }
  S_OK = HRESULT(0);
  S_FALSE = HRESULT(1);
  E_NOTIMPL = HRESULT($80004001);
  E_NOINTERFACE = HRESULT($80004002);
  E_FAIL = HRESULT($80004005);

var
  hr: HRESULT;

begin
  { Test that the constants have expected values }
  hr := VAR_OK;
  if hr <> 0 then
    writeln('FAIL: VAR_OK should be 0')
  else
    writeln('PASS: VAR_OK = 0');

  hr := S_OK;
  if hr <> 0 then
    writeln('FAIL: S_OK should be 0')
  else
    writeln('PASS: S_OK = 0');

  hr := S_FALSE;
  if hr <> 1 then
    writeln('FAIL: S_FALSE should be 1')
  else
    writeln('PASS: S_FALSE = 1');

  { HRESULT is signed, so high bit values become negative }
  hr := VAR_PARAMNOTFOUND;
  if hr >= 0 then
    writeln('FAIL: VAR_PARAMNOTFOUND should be negative (high bit set)')
  else
    writeln('PASS: VAR_PARAMNOTFOUND is negative');

  hr := E_FAIL;
  if hr >= 0 then
    writeln('FAIL: E_FAIL should be negative')
  else
    writeln('PASS: E_FAIL is negative');

  writeln('HRESULT const typecast test completed');
end.
