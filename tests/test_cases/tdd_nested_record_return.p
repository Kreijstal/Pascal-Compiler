program tdd_nested_record_return;
{$mode objfpc}

Type
  TOuter = class
  private
    type
      TNested = record
        F: Integer;
      end;
  public
    function GetIt: TNested;
  end;

function TOuter.GetIt: TNested;
begin
  Result.F := 123;
end;

var
  O: TOuter;
  R: TOuter.TNested;

begin
  O := TOuter.Create;
  R := O.GetIt;
  writeln(R.F);
end.
