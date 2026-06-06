program fpc_bootstrap_pbestrealtype_deref_assign;

{$mode objfpc}

uses
  fpc_bootstrap_pbestrealtype_defs;

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
