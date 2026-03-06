unit ubase;

{$mode objfpc}

interface

uses unixType;

function UseSize(n: TSize): QWord;

implementation

function OverSize(n: QWord): QWord;
begin
  Result := n;
end;

function UseSize(n: TSize): QWord;
begin
  Result := OverSize(n);
end;

end.
