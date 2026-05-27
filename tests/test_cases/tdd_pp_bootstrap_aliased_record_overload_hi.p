unit tdd_pp_bootstrap_aliased_record_overload_hi;
{ Higher-layer unit: declares its own canonical record AND a transparent
  alias that REUSES the same identifier `TSharedRec` already declared
  in the lower-layer unit.  This produces two distinct RecordType nodes
  in the symbol table for the same name — exactly the configuration
  that broke FPC pp.pas --target=windows for FileTimeToSystemTime. }
interface

uses tdd_pp_bootstrap_aliased_record_overload_lo;

type
  TCanonical = record
    a, b: LongWord;
  end;
  TSharedRec = TCanonical; { transparent alias under the same name as
                             the record in the lower-layer unit }

procedure Stamp(var R: TSharedRec); overload;
procedure Stamp(x: LongInt); overload;

implementation

procedure Stamp(var R: TSharedRec); overload;
begin
  R.a := R.a + 1;
  R.b := R.b + 2;
end;

procedure Stamp(x: LongInt); overload;
begin
end;

end.
