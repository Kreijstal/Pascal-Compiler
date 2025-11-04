program OperatorOverloadDemo;

{ This program demonstrates operator overloading support in the compiler.
  It shows both FPC-style (using symbols) and Delphi-style (using names) operator syntax.
  
  NOTE: This test only validates parsing. Full execution support requires:
  - Semantic checking of operator declarations
  - Operator resolution in expressions  
  - Code generation for operator calls
}

type
  { Simple 2D vector with FPC-style operator overloading }
  TVector = record
    X, Y: Integer;
    class operator +(const A, B: TVector): TVector;
    class operator -(const A, B: TVector): TVector;
    class operator *(const V: TVector; Scalar: Integer): TVector;
  end;

  { 2x2 matrix with Delphi-style operator overloading }
  TMatrix = record
    A11, A12, A21, A22: Integer;
    class operator Add(const M1, M2: TMatrix): TMatrix;
    class operator Subtract(const M1, M2: TMatrix): TMatrix;
  end;

{ FPC-style operator implementations using symbols }
class operator TVector.+(const A, B: TVector): TVector;
begin
  { Note: Without Result support, we'd need to assign to TVector.+ which doesn't parse }
  { For now, this is just a parsing test }
end;

class operator TVector.-(const A, B: TVector): TVector;
begin
end;

class operator TVector.*(const V: TVector; Scalar: Integer): TVector;
begin
end;

{ Delphi-style operator implementations using names }
class operator TMatrix.Add(const M1, M2: TMatrix): TMatrix;
begin
end;

class operator TMatrix.Subtract(const M1, M2: TMatrix): TMatrix;
begin
end;

begin
  WriteLn('Operator overloading parsing test passed!');
end.
