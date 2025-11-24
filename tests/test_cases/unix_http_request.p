{$mode objfpc}
{$H+}

program unix_http_request;

{ Demonstrates how to perform a simple HTTP GET request using raw
  Unix sockets via libc from Free Pascal. }

uses
  SysUtils;

const
  libc = 'c';
  TargetHost = 'httpbin.org';
  TargetPort = 80;
  MaxChunkSize = 2048;
  AF_INET = 2;
  SOCK_STREAM = 1;

type
  PPAnsiChar = ^PAnsiChar;
  PByte = ^Byte;
  TInt = LongInt;
  TSize = NativeUInt;
  TSSize = NativeInt;

  TInAddr = record
    S_addr: LongWord;
  end;
  PInAddr = ^TInAddr;

  TInetSockAddr = packed record
    sin_family: Word;
    sin_port: Word;
    sin_addr: TInAddr;
    sin_zero: array[0..7] of Byte;
  end;

  THostEnt = record
    h_name: PAnsiChar;
    h_aliases: PPAnsiChar;
    h_addrtype: SmallInt;
    h_length: SmallInt;
    h_addr_list: PPAnsiChar;
  end;
  PHostEnt = ^THostEnt;

function socket(domain, typ, protocol: TInt): TInt; cdecl; external libc;
function connect(sockfd: TInt; addr: Pointer; len: NativeUInt): TInt; cdecl; external libc;
function send(sockfd: TInt; buf: Pointer; len: TSize; flags: TInt): TSSize; cdecl; external libc;
function recv(sockfd: TInt; buf: Pointer; len: TSize; flags: TInt): TSSize; cdecl; external libc;
function libc_close(sockfd: TInt): TInt; cdecl; external libc name 'close';
function gethostbyname(name: PAnsiChar): PHostEnt; cdecl; external libc;
function htons(value: Word): Word; cdecl; external libc;
function inet_ntoa(in_addr: TInAddr): PAnsiChar; cdecl; external libc;

var
  Sock: TInt = -1;
  Server: PHostEnt;
  Addr: TInetSockAddr;
  Request, ResponseChunk: AnsiString;
  SentBytes, ReceivedBytes: TSSize;
  Buffer: array[0..MaxChunkSize - 1] of AnsiChar;
  IPStr: PAnsiChar;

begin
  try
    try
      Writeln('Resolving host...');
      Server := gethostbyname(PAnsiChar(TargetHost));
      if Server = nil then
        raise Exception.Create('Unable to resolve host ' + TargetHost);
      Writeln('Host resolved.');

      Writeln('Creating socket...');
      Sock := socket(AF_INET, SOCK_STREAM, 0);
      if Sock < 0 then
        raise Exception.Create('Could not create socket');
      Writeln('Socket created.');

      FillChar(Addr, SizeOf(Addr), 0);
      Addr.sin_family := AF_INET;
      Addr.sin_port := htons(TargetPort);
      Addr.sin_addr := PInAddr(Server^.h_addr_list^)^;

      IPStr := inet_ntoa(Addr.sin_addr);
      Writeln('Target IP: ', IPStr);
      Writeln('Target Port (Network Order): ', Addr.sin_port);

      Writeln('Connecting...');
      if connect(Sock, @Addr, SizeOf(Addr)) < 0 then
        raise Exception.Create('Could not connect to ' + TargetHost);
      Writeln('Connected.');

      Request := 'GET /ip HTTP/1.1' + #13#10 + 'Host: ' + TargetHost + #13#10 + 'Connection: close' + #13#10 + #13#10;
      Writeln('Sending request...');
      SentBytes := send(Sock, PAnsiChar(Request), Length(Request), 0);
      if SentBytes < Length(Request) then
        raise Exception.Create('Could not send entire request');
      Writeln('Request sent.');

      Writeln('Receiving response...');
      repeat
        ReceivedBytes := recv(Sock, @Buffer[0], MaxChunkSize, 0);
        if ReceivedBytes > 0 then
        begin
          SetString(ResponseChunk, PAnsiChar(@Buffer[0]), ReceivedBytes);
          write(ResponseChunk);
        end
        else if ReceivedBytes < 0 then
          raise Exception.Create('Error while receiving response');
      until ReceivedBytes <= 0;
      Writeln('Response received.');
    except
      on E: Exception do
        Writeln('Error: ', E.Message);
    end;
  finally
    if Sock >= 0 then
    begin
      Writeln('Closing socket...');
      libc_close(Sock);
    end;
  end;
end.
