{$mode objfpc}
{$H+}

program unix_http_request;

{ Demonstrates how to perform a simple HTTP GET request using raw
Unix sockets via libc from Free Pascal. }

uses
SysUtils;

const
libc = 'c';
TargetHost = 'localhost';
TargetPort = 8080;
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
function close(sockfd: TInt): TInt; cdecl; external libc;
function gethostbyname(name: PAnsiChar): PHostEnt; cdecl; external libc;
function htons(value: Word): Word; cdecl; external libc;

var
Sock: TInt = -1;
Server: PHostEnt;
Addr: TInetSockAddr;
Request, ResponseChunk: AnsiString;
SentBytes, ReceivedBytes: TSSize;
Buffer: array[0..MaxChunkSize - 1] of AnsiChar;

begin
try
try
Server := gethostbyname(PAnsiChar(TargetHost));
if Server = nil then
raise Exception.Create('Unable to resolve host ' + TargetHost);

Sock := socket(AF_INET, SOCK_STREAM, 0);
if Sock < 0 then
raise Exception.Create('Could not create socket');

FillChar(Addr, SizeOf(Addr), 0);
Addr.sin_family := AF_INET;
Addr.sin_port := htons(TargetPort);
Addr.sin_addr := PInAddr(Server^.h_addr_list^)^;

if connect(Sock, @Addr, SizeOf(Addr)) < 0 then
raise Exception.Create('Unable to connect to ' + TargetHost);

Request := AnsiString(Format(
'GET /get HTTP/1.1'#13#10 +
'Host: %s'#13#10 +
'User-Agent: FPC Unix Socket Sample'#13#10 +
'Connection: close'#13#10#13#10,
[TargetHost]
));

SentBytes := send(Sock, PAnsiChar(Request), Length(Request), 0);
if SentBytes <> Length(Request) then
raise Exception.Create('Failed to send entire request');

writeln('Successfully sent request to ', TargetHost, ':', TargetPort);
writeln('Response:');

repeat
ReceivedBytes := recv(Sock, @Buffer[0], SizeOf(Buffer), 0);
if ReceivedBytes > 0 then
begin
SetString(ResponseChunk, PAnsiChar(@Buffer[0]), ReceivedBytes);
write(ResponseChunk);
end
else if ReceivedBytes < 0 then
raise Exception.Create('Error while receiving response');
until ReceivedBytes <= 0;
except
on E: Exception do
Writeln('Error: ', E.Message);
end;
finally
if Sock >= 0 then
close(Sock);
end;
end.