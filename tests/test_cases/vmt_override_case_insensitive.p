{ Regression test: virtual override matching must be case-insensitive on
  parameter type names, since Pascal identifiers are case-insensitive.

  Bug: KGPC's build_class_vmt compared param_sig with strcmp.  Parent
  classes that declared parameters with one case (e.g. lowercase
  "tcgsize") and child overrides that used a different case ("TCGSize")
  failed to match — the override was appended as a NEW VMT slot instead
  of replacing the inherited one.  Calls through a base-class pointer
  then dispatched to the parent method, missing the override.

  In FPC pp.pas: tcg.a_bit_scan_reg_reg (cgobj.pas, tcgsize) vs
  tcgx86.a_bit_scan_reg_reg (cgx86.pas, TCGSize) failed to bind, so
  every BsfQWord(longint) call landed in tcg.a_bit_scan_reg_reg which
  raises internalerror(2014070601). }
program vmt_override_case_insensitive;
{$mode objfpc}

type
  tmyenum = (m_a, m_b, m_c);

  TBase = class
    procedure pre1; virtual;
    procedure pre2; virtual;
    procedure target(a: tmyenum; b: tmyenum); virtual;  { lowercase type spelling }
    procedure post1; virtual;
  end;

  TMid = class(TBase)
    { Same method, override uses UPPERCASE spelling of the same type }
    procedure target(a: TMyEnum; b: TMyEnum); override;
  end;

  TTop = class(TMid)
    procedure top_extra; virtual;
  end;

procedure TBase.pre1; begin end;
procedure TBase.pre2; begin end;
procedure TBase.target(a: tmyenum; b: tmyenum);
  begin writeln('FAIL: TBase.target reached'); halt(1); end;
procedure TBase.post1; begin end;
procedure TMid.target(a: TMyEnum; b: TMyEnum);
  begin writeln('ok ', ord(a), ' ', ord(b)); end;
procedure TTop.top_extra; begin end;

var
  b: TBase;
begin
  b := TTop.Create;
  b.target(m_a, m_c);
  b.Free;
end.
