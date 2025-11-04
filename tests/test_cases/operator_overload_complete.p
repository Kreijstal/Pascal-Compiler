program OperatorOverloadingComplete;

{ This program demonstrates complete operator overloading support with var sections.
  It shows both FPC-style (using symbols) and Delphi-style (using names) operator syntax
  with local variable declarations in the operator implementations. }

type
  { Simple 2D vector with FPC-style operator overloading }
  TVector = record
    X, Y: Single;
    class operator +(const A, B: TVector): TVector;
    class operator -(const A, B: TVector): TVector;
    class operator *(const V: TVector; const Scalar: Single): TVector;
  end;

  { 2x2 matrix with Delphi-style operator overloading }
  TMatrix = record
    A11, A12, A21, A22: Single;
    class operator Add(const M1, M2: TMatrix): TMatrix;
    class operator Multiply(const M1, M2: TMatrix): TMatrix;
  end;

{ FPC-style operator implementations with var sections }
class operator TVector.+(const A, B: TVector): TVector;
var
  Result: TVector;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  { Note: Returning Result requires proper Result variable support or
    assigning to the function name, which needs special handling for operators }
end;

class operator TVector.-(const A, B: TVector): TVector;
var
  Result: TVector;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TVector.*(const V: TVector; const Scalar: Single): TVector;
var
  Result: TVector;
begin
  Result.X := V.X * Scalar;
  Result.Y := V.Y * Scalar;
end;

{ Delphi-style operator implementations with var sections }
class operator TMatrix.Add(const M1, M2: TMatrix): TMatrix;
var
  Result: TMatrix;
begin
  Result.A11 := M1.A11 + M2.A11;
  Result.A12 := M1.A12 + M2.A12;
  Result.A21 := M1.A21 + M2.A21;
  Result.A22 := M1.A22 + M2.A22;
end;

class operator TMatrix.Multiply(const M1, M2: TMatrix): TMatrix;
var
  Result: TMatrix;
begin
  { Matrix multiplication: C = A * B }
  Result.A11 := M1.A11 * M2.A11 + M1.A12 * M2.A21;
  Result.A12 := M1.A11 * M2.A12 + M1.A12 * M2.A22;
  Result.A21 := M1.A21 * M2.A11 + M1.A22 * M2.A21;
  Result.A22 := M1.A21 * M2.A12 + M1.A22 * M2.A22;
end;

begin
  WriteLn('Operator overloading with var sections parsed successfully!');
  WriteLn('This demonstrates:');
  WriteLn('  - FPC-style operators (+, -, *)');
  WriteLn('  - Delphi-style operators (Add, Multiply)');
  WriteLn('  - Local var sections in operator implementations');
end.
