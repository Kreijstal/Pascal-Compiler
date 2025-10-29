program TypeAliasParameterCalls;

type
  TChar = char;
  TIntPtr = ^integer;
  TPalette = (Red, Green, Blue);
  TPaletteSet = set of TPalette;
  TCharFile = file;

procedure UseChar(value: TChar);
begin
end;

procedure UsePointer(value: TIntPtr);
begin
end;

procedure UseEnum(value: TPalette);
begin
end;

procedure UseSet(value: TPaletteSet);
begin
end;

procedure UseFile(var value: TCharFile);
begin
end;

var
  ch: char;
  ptr: TIntPtr;
  color: TPalette;
  colors: TPaletteSet;
  fileVar: TCharFile;

begin
  UseChar(ch);
  UsePointer(ptr);
  UseEnum(color);
  UseSet(colors);
  UseFile(fileVar);
end.
