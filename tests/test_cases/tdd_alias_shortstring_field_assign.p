program tdd_alias_shortstring_field_assign;
{$mode objfpc}{$H+}

{ Regression: a record/class field declared with a *named alias* to a
  ShortString (`TString80 = string[80]`) must be assigned from an
  AnsiString through the ShortString copy that writes the length byte,
  not the raw char-array copy.

  The alias `string[N]` form was resolved by convert_type_spec_to_kgpctype
  to a managed AnsiString primitive, dropping the [N] bound and the
  ShortString storage kind.  Fields of the alias type therefore looked
  like plain `array[0..N] of Char`, so `field := someAnsiString` took
  kgpc_string_to_char_array (no length byte): the first data char landed
  in the length-byte slot and the field decoded as garbage.

  This was the root cause of the intermittent, Win64-surfaced 1-byte
  clobber of TObjData.FName (`FName := ExtractFileName(n)` in
  FPCSource/compiler/ogbase.pas), which broke the KGPC->FPC Windows
  self-host (Internal error 202102001). }

type
  TString80 = string[80];     { named alias to a ShortString }
  TRec = record
    Name : TString80;         { field via the alias }
  end;

function MakeName(const n: string): ansistring;
begin
  Result := n;
end;

var
  r: TRec;
begin
  r.Name := MakeName('system.o');
  writeln(r.Name);
end.
