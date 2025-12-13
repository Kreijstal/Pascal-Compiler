{ Test for FPC bootstrap: forward-declared procedure addresses in typed constants }
{ This pattern is used in FPC's sortbase.pp for TSortingAlgorithm record }
{$mode objfpc}
program fpc_bootstrap_forward_proc_const;

type
  TCompareFunc = function(a, b: Integer): Integer;
  
  TSortAlgo = record
    Compare: TCompareFunc;
  end;

{ Forward declare procedures that will be referenced in the const }
function CompareAsc(a, b: Integer): Integer; forward;
function CompareDesc(a, b: Integer): Integer; forward;

{ Use forward-declared procedures in typed constant }
const
  AscendingSort: TSortAlgo = (
    Compare: @CompareAsc
  );
  
  DescendingSort: TSortAlgo = (
    Compare: @CompareDesc
  );

{ Implement the forward-declared procedures }
function CompareAsc(a, b: Integer): Integer;
begin
  Result := a - b;
end;

function CompareDesc(a, b: Integer): Integer;
begin
  Result := b - a;
end;

begin
  { Test that the const record has the correct procedure addresses }
  WriteLn(AscendingSort.Compare(5, 3));   { Should print 2 }
  WriteLn(DescendingSort.Compare(5, 3));  { Should print -2 }
end.
