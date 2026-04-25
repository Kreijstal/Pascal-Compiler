program class_field_virtual_function_repro;
{$mode objfpc}
type
  TFile = class
    Name: string;
    constructor Create(const fn: string);
    function CreateFile: Boolean; virtual;
  end;

  TCompilerFile = class(TFile)
  end;

  TModule = class
    FileObj: TCompilerFile;
    procedure Build(const s: string);
  end;

constructor TFile.Create(const fn: string);
begin
  Name := fn;
end;

function TFile.CreateFile: Boolean;
begin
  Result := Name = 'x';
end;

procedure TModule.Build(const s: string);
begin
  FileObj := TCompilerFile.Create(s);
  if FileObj = nil then
    WriteLn('nil')
  else
  begin
    WriteLn('assigned');
    WriteLn('name=', FileObj.Name);
    if FileObj.CreateFile then
      WriteLn('createfile=true')
    else
      WriteLn('createfile=false');
    WriteLn('after=', FileObj.Name);
  end;
end;

var
  m: TModule;
begin
  m := TModule.Create;
  m.Build('x');
end.
