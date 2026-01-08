{
  Test case: Calling abstract method from base class fails.

  BUG: KGPC cannot resolve calls to abstract methods from within the same
  class that declares them.
  Error: "call to function GetValue does not match any available overload"

  This blocks FPC bootstrap because finput.pas and many other units use:
    function fileopen(...): boolean; virtual; abstract;
  and call these methods from non-abstract methods in the same class.

  FPC correctly resolves abstract method calls within the declaring class.
}
program fpc_bootstrap_abstract_method_call;
{$mode objfpc}

type
  TBase = class
  protected
    function GetValue(const x: String): Integer; virtual; abstract;
  public
    function UseValue: Integer;
  end;

  TDerived = class(TBase)
  protected
    function GetValue(const x: String): Integer; override;
  end;

function TBase.UseValue: Integer;
begin
  Result := GetValue('hello');  { Fails in KGPC: cannot find GetValue }
end;

function TDerived.GetValue(const x: String): Integer;
begin
  Result := Length(x);
end;

var
  obj: TDerived;
begin
  obj := TDerived.Create;
  WriteLn(obj.UseValue);
  obj.Free;
end.
