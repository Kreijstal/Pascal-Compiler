program tdd_generic_nested_case_labels;

{$mode objfpc}
{$modeswitch generics}

type
  TCallConv = (ccReg, ccCdecl, ccOldFPCCall, ccPascal, ccStdCall, ccCppdecl, ccMWPascal);

generic function ConstParamIsRef<T>(aCallConv: TCallConv): Boolean;

  function SameAddrRegister(const aArg1: T; constref aArg2: T): Boolean; register;
  begin
    Result := @aArg1 = @aArg2;
  end;

  function SameAddrCDecl(const aArg1: T; constref aArg2: T): Boolean; cdecl;
  begin
    Result := @aArg1 = @aArg2;
  end;

  function SameAddrStdCall(const aArg1: T; constref aArg2: T): Boolean; stdcall;
  begin
    Result := @aArg1 = @aArg2;
  end;

  function SameAddrCppDecl(const aArg1: T; constref aArg2: T): Boolean; cppdecl;
  begin
    Result := @aArg1 = @aArg2;
  end;

  function SameAddrMWPascal(const aArg1: T; constref aArg2: T): Boolean; mwpascal;
  begin
    Result := @aArg1 = @aArg2;
  end;

var
  v: T;
begin
  v := Default(T);
  case aCallConv of
    ccReg:
      Result := SameAddrRegister(v, v);
    ccCdecl:
      Result := SameAddrCDecl(v, v);
    ccOldFPCCall,
    ccPascal,
    ccStdCall:
      Result := SameAddrStdCall(v, v);
    ccCppdecl:
      Result := SameAddrCppDecl(v, v);
    ccMWPascal:
      Result := SameAddrMWPascal(v, v);
    else
      Result := False;
  end;
end;

begin
  if specialize ConstParamIsRef<Longint>(ccStdCall) then
    writeln('true')
  else
    writeln('false');
end.
