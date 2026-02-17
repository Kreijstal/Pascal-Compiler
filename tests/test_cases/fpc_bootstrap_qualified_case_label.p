program fpc_bootstrap_qualified_case_label;
{$mode objfpc}
{$scopedenums on}

type
  THorzRectAlign = (Left, Center, Right);
  TVertRectAlign = (Top, Center, Bottom);

procedure TestHorz(AHorzAlign: THorzRectAlign);
begin
  case AHorzAlign of
    THorzRectAlign.Left:
      WriteLn('Left');
    THorzRectAlign.Center:
      WriteLn('HCenter');
    THorzRectAlign.Right:
      WriteLn('Right');
  end;
end;

procedure TestVert(AVertAlign: TVertRectAlign);
begin
  case AVertAlign of
    TVertRectAlign.Top:
      WriteLn('Top');
    TVertRectAlign.Center:
      WriteLn('VCenter');
    TVertRectAlign.Bottom:
      WriteLn('Bottom');
  end;
end;

begin
  TestHorz(THorzRectAlign.Left);
  TestHorz(THorzRectAlign.Center);
  TestHorz(THorzRectAlign.Right);
  TestVert(TVertRectAlign.Top);
  TestVert(TVertRectAlign.Center);
  TestVert(TVertRectAlign.Bottom);
end.
