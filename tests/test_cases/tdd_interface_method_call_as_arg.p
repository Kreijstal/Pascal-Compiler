program tdd_interface_method_call_as_arg;

{$mode objfpc}

type
  HRESULT = LongInt;
  DWORD = Cardinal;
  PDWORD = ^DWORD;

  ITestStream = interface
    function Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HRESULT; stdcall;
  end;

  TMockStream = class(TInterfacedObject, ITestStream)
    function Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HRESULT; stdcall;
  end;

  TProxyStream = class
  private
    FStream: ITestStream;
    procedure Check(err: Integer);
  public
    constructor Create(AStream: ITestStream);
    function Read(var Buffer; Count: Longint): Longint;
  end;

function TMockStream.Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HRESULT; stdcall;
begin
  if pcbRead <> nil then
    pcbRead^ := cb;
  Result := 0;
end;

procedure TProxyStream.Check(err: Integer);
begin
  if err < 0 then
    WriteLn('error')
  else
    WriteLn('ok');
end;

constructor TProxyStream.Create(AStream: ITestStream);
begin
  inherited Create;
  FStream := AStream;
end;

function TProxyStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  Check(FStream.Read(@Buffer, Count, @Result));
end;

var
  test: TProxyStream;
  buf: Integer;
begin
  test := TProxyStream.Create(TMockStream.Create);
  buf := 0;
  WriteLn(test.Read(buf, 42));
  test.Free;
end.
