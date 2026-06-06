program fpc_bootstrap_shortstring_field_index;

{$H-}

type
  TOptions = class
    QuickInfo: string;
    procedure SetInfo(const S: string);
    procedure PrintFirst;
  end;

procedure TOptions.SetInfo(const S: string);
begin
  QuickInfo := S;
end;

procedure TOptions.PrintFirst;
begin
  Writeln(QuickInfo[1]);
end;

var
  Opt: TOptions;

begin
  Opt := TOptions.Create;
  Opt.SetInfo('TP');
  Opt.PrintFirst;
  Opt.Free;
end.
