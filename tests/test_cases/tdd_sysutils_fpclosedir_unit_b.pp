{$mode objfpc}
unit tdd_sysutils_fpclosedir_unit_b;

interface

uses
  tdd_sysutils_fpclosedir_unit_a;

procedure FpClosedir(dirp: PDirRec); overload;
function FpReaddir(dirp: PDirRec): longint; overload;

implementation

procedure FpClosedir(dirp: PDirRec); overload;
var
  i: integer;
begin
  for i := 0 to High(dirp^.hist) do
    dirp^.hist[i] := dirp^.hist[i] - 1000;
  dirp^.payload.tag := dirp^.payload.tag + 'X';
end;

function FpReaddir(dirp: PDirRec): longint; overload;
begin
  Result := -dirp^.count;
  dirp^.count := dirp^.count - 9;
end;

end.
