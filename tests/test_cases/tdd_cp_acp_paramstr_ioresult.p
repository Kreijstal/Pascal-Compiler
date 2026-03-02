program tdd_cp_acp_paramstr_ioresult;

{$I-}

var
  f: text;
  s: string;
  c: longint;
  r: longint;

begin
  s := ParamStr(0);
  c := ParamCount;
  assign(f, 'kgpc_tdd_missing_dir_12345/never_exists.tmp');
  reset(f);
  r := IOResult;
  writeln('io1=', r);
  writeln('cp=', CP_ACP);
  writeln('io2=', IOResult);
end.
