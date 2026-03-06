program method_driver_read_write_not_builtin;
{$mode objfpc}

 type
  TDriver = class
    procedure Read(var Buf; Count: LongInt);
    procedure Write(const Buf; Count: LongInt);
  end;

  TReaderLike = class
  private
    FDriver: TDriver;
    function GetDriver: TDriver;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    property Driver: TDriver read GetDriver;
  end;

procedure TDriver.Read(var Buf; Count: LongInt);
var
  P: PByte;
begin
  P := @Buf;
  if Count > 0 then
    P^ := 3;
end;

procedure TDriver.Write(const Buf; Count: LongInt);
var
  P: PByte;
begin
  P := @Buf;
  if Count > 0 then
    WriteLn(P^);
end;

constructor TReaderLike.Create;
begin
  inherited Create;
  FDriver := TDriver.Create;
end;

destructor TReaderLike.Destroy;
begin
  FDriver.Free;
  inherited Destroy;
end;

function TReaderLike.GetDriver: TDriver;
begin
  Result := FDriver;
end;

procedure TReaderLike.Run;
var
  B: Byte;
begin
  B := 0;
  Driver.Read(B, 1);
  Driver.Write(B, 1);
end;

var
  R: TReaderLike;
begin
  R := TReaderLike.Create;
  R.Run;
  R.Free;
end.
