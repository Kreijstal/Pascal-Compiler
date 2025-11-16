{$mode objfpc}
{$H+}

program http_server;

uses
  SysUtils;

const
  libc = 'c';
  Port = 8080;
  MaxConnections = 1;
  AF_INET = 2;
  SOCK_STREAM = 1;
  INADDR_ANY = 0;

type
  TInt = LongInt;
  TSize = NativeUInt;
  TSSize = NativeInt;
  TInetSockAddr = packed record
    sin_family: Word;
    sin_port: Word;
    sin_addr: record S_addr: LongWord; end;
    sin_zero: array[0..7] of Byte;
  end;
  socklen_t = Cardinal;

function socket(domain, typ, protocol: TInt): TInt; cdecl; external libc;
function bind(sockfd: TInt; addr: Pointer; len: socklen_t): TInt; cdecl; external libc;
function listen(sockfd: TInt; backlog: TInt): TInt; cdecl; external libc;
function accept(sockfd: TInt; addr: Pointer; len: Pointer): TInt; cdecl; external libc;
function recv(sockfd: TInt; buf: Pointer; len: TSize; flags: TInt): TSSize; cdecl; external libc;
function send(sockfd: TInt; buf: Pointer; len: TSize; flags: TInt): TSSize; cdecl; external libc;
function close(sockfd: TInt): TInt; cdecl; external libc;
function htons(value: Word): Word; cdecl; external libc;

var
  ListenSocket, ClientSocket: TInt;
  ClientAddr: TInetSockAddr;
  AddrLen: socklen_t;
  Buffer: array[0..2047] of AnsiChar;
  BytesRead: TSSize;
  Response: AnsiString;

begin
  writeln('Starting server on port ', Port);

  ListenSocket := socket(AF_INET, SOCK_STREAM, 0);
  if ListenSocket < 0 then
  begin
    writeln('Error: Could not create socket');
    Halt(1);
  end;

  try
    FillChar(ClientAddr, SizeOf(ClientAddr), 0);
    ClientAddr.sin_family := AF_INET;
    ClientAddr.sin_port := htons(Port);
    ClientAddr.sin_addr.S_addr := INADDR_ANY;

    if bind(ListenSocket, @ClientAddr, SizeOf(ClientAddr)) < 0 then
    begin
      writeln('Error: Could not bind socket');
      Halt(1);
    end;

    if listen(ListenSocket, MaxConnections) < 0 then
    begin
      writeln('Error: Could not listen on socket');
      Halt(1);
    end;

    writeln('Server listening on port ', Port);

    AddrLen := SizeOf(ClientAddr);
    ClientSocket := accept(ListenSocket, @ClientAddr, @AddrLen);

    if ClientSocket < 0 then
    begin
      writeln('Error: Could not accept connection');
      Halt(1);
    end;

    writeln('Client connected');

    BytesRead := recv(ClientSocket, @Buffer[0], SizeOf(Buffer), 0);
    if BytesRead > 0 then
    begin
      writeln('Received request:');
      write(Buffer, BytesRead);
    end;

    Response := 'HTTP/1.0 200 OK' + #13#10 +
                'Content-Type: text/plain' + #13#10 +
                'Content-Length: 13' + #13#10 +
                #13#10 +
                'Hello, world!';

    send(ClientSocket, PAnsiChar(Response), Length(Response), 0);
    writeln('Sent response');

  finally
    if ClientSocket >= 0 then
      close(ClientSocket);
    if ListenSocket >= 0 then
      close(ListenSocket);
  end;
end.