{$mode objfpc}
unit tdd_sysutils_fpclosedir_unit_a;

interface

uses
  SysUtils;

type
  TPayload = record
    weights: array[0..7] of longint;
    tag: string;
    scale: longint;
  end;

  TDirRec = record
    count: longint;
    payload: TPayload;
    hist: array[0..3] of longint;
  end;

  PDirRec = ^TDirRec;

procedure FpClosedir(var dirp: TDirRec); overload;
function FpReaddir(var dirp: TDirRec): longint; overload;
procedure InitDir(var d: TDirRec);
function ComputeChecksum(const d: TDirRec): longint;

implementation

procedure FpClosedir(var dirp: TDirRec); overload;
var
  i: integer;
begin
  dirp.count := dirp.count + 17;
  for i := 0 to High(dirp.payload.weights) do
    dirp.payload.weights[i] := dirp.payload.weights[i] + dirp.count + i;
  dirp.payload.tag := dirp.payload.tag + '#' + IntToStr(dirp.count);
  for i := 0 to High(dirp.hist) do
    dirp.hist[i] := dirp.hist[i] + dirp.payload.weights[i] div (i + 1);
end;

function FpReaddir(var dirp: TDirRec): longint; overload;
var
  i: integer;
  acc: longint;
begin
  acc := 0;
  for i := 0 to High(dirp.payload.weights) do
    acc := acc + (dirp.payload.weights[i] xor (i + dirp.count));
  dirp.count := dirp.count + acc mod 7;
  Result := acc;
end;

procedure InitDir(var d: TDirRec);
var
  i: integer;
begin
  d.count := 3;
  d.payload.tag := 'seed';
  d.payload.scale := 5;
  for i := 0 to High(d.payload.weights) do
    d.payload.weights[i] := (i + 1) * d.payload.scale + i * i;
  for i := 0 to High(d.hist) do
    d.hist[i] := i * 7;
end;

function ComputeChecksum(const d: TDirRec): longint;
var
  i: integer;
  acc: longint;
begin
  acc := Length(d.payload.tag) * 13 + d.count;
  for i := 0 to High(d.payload.weights) do
    acc := acc + d.payload.weights[i] * (i + 1);
  for i := 0 to High(d.hist) do
    acc := acc + d.hist[i] * (i + 2);
  Result := acc;
end;

end.
