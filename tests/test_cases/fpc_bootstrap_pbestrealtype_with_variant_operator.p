program fpc_bootstrap_pbestrealtype_with_variant_operator;

{$mode objfpc}

uses
  fpc_bootstrap_pbestrealtype_defs;

type
  TError = type LongInt;
  OleVariant = type LongInt;

operator := (const source: OleVariant): TError;
begin
  Result := 0;
end;

type
  TNode = class(TBaseNode)
    procedure SetBest;
  end;

procedure TNode.SetBest;
begin
  resultdef := pbestrealtype^;
end;

var
  node: TNode;

begin
  bestrealtype := TFloatDef.Create('s64real');
  node := TNode.Create;
  node.SetBest;
  WriteLn(node.resultdef.name);
end.
