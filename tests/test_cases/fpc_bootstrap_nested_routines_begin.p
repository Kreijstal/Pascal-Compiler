{ Repro for KGPC: nested routine declarations before the outer begin are misparsed. }
{ FPC compiles this. KGPC currently fails with: }
{   "Unexpected content before final '.'" at the nested function declaration. }

program fpc_bootstrap_nested_routines_begin;

function Outer: Integer;
var
  Format: PChar;

procedure InitVars;
begin
end;

function GetSections(Var SP: array[0..3] of Integer): Integer;
var
  C,Q: Char;
  InQuote: Boolean;
  I, FL: Integer;
begin
  Result := 1;
  InQuote := False;
  Q := #0;
  I := 0;
  FL := 0;
  while (I < FL) do
    begin
      C := Format[I];
      case C of
        ';':
          begin
            if not InQuote then
              begin
                SP[Result] := I + 1;
                Inc(Result);
              end;
          end;
        '"','''':
          begin
            if InQuote then
              InQuote := C <> Q
            else
              begin
                InQuote := True;
                Q := C;
              end;
          end;
      end;
      Inc(I);
    end;
end;

begin
  Result := 7;
end;

begin
  Writeln(Outer);
end.
