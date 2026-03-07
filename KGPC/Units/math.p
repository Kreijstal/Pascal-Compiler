unit Math;

interface

const
    Pi = 3.14159265358979323846;
    MaxDouble = 1.7976931348623157e308;

function Sqrt(Value: Real): Real; cdecl; external name 'kgpc_sqrt';
function Sin(Value: Real): Real; cdecl; external name 'kgpc_sin';
function Cos(Value: Real): Real; cdecl; external name 'kgpc_cos';
function Tan(Value: Real): Real; cdecl; external name 'kgpc_tan';
function Cot(Value: Real): Real; cdecl; external name 'kgpc_cot';
function Sec(Value: Real): Real; cdecl; external name 'kgpc_sec';
function Csc(Value: Real): Real; cdecl; external name 'kgpc_csc';
function ArcSin(Value: Real): Real; cdecl; external name 'kgpc_arcsin';
function ArcCos(Value: Real): Real; cdecl; external name 'kgpc_arccos';
function ArcTan(Value: Real): Real; cdecl; external name 'kgpc_arctan';
function ArcTan2(Y, X: Real): Real; cdecl; external name 'kgpc_arctan2';
function ArcCot(Value: Real): Real; cdecl; external name 'kgpc_arccot';
function Sinh(Value: Real): Real; cdecl; external name 'kgpc_sinh';
function Cosh(Value: Real): Real; cdecl; external name 'kgpc_cosh';
function Tanh(Value: Real): Real; cdecl; external name 'kgpc_tanh';
function Coth(Value: Real): Real; cdecl; external name 'kgpc_coth';
function Sech(Value: Real): Real; cdecl; external name 'kgpc_sech';
function Csch(Value: Real): Real; cdecl; external name 'kgpc_csch';
function ArcSinh(Value: Real): Real; cdecl; external name 'kgpc_arcsinh';
function ArcCosh(Value: Real): Real; cdecl; external name 'kgpc_arccosh';
function ArcTanh(Value: Real): Real; cdecl; external name 'kgpc_arctanh';
function ArcCoth(Value: Real): Real; cdecl; external name 'kgpc_arccoth';
function ArcSech(Value: Real): Real; cdecl; external name 'kgpc_arcsech';
function ArcCsch(Value: Real): Real; cdecl; external name 'kgpc_arccsch';
function Exp(Value: Real): Real; cdecl; external name 'kgpc_exp';
function Ln(Value: Real): Real; cdecl; external name 'kgpc_ln';
function LogN(Base, Value: Real): Real; cdecl; external name 'kgpc_logn';
function Hypot(X, Y: Real): Real; cdecl; external name 'kgpc_hypot';
function Power(Base, Exponent: Real): Real; cdecl; external name 'kgpc_power';
function Round(Value: Real): Longint; cdecl; external name 'kgpc_round';
function Trunc(Value: Real): Longint; cdecl; external name 'kgpc_trunc';
function Int(Value: Real): Longint; cdecl; external name 'kgpc_int';
function Frac(Value: Real): Real; cdecl; external name 'kgpc_frac';
function Ceil(Value: Real): Longint; cdecl; external name 'kgpc_ceil';
function Floor(Value: Real): Longint; cdecl; external name 'kgpc_floor';
function DegToRad(Degrees: Real): Real; cdecl; external name 'kgpc_deg_to_rad';
function RadToDeg(Radians: Real): Real; cdecl; external name 'kgpc_rad_to_deg';
function DegToGrad(Degrees: Real): Real; cdecl; external name 'kgpc_deg_to_grad';
function GradToDeg(Grads: Real): Real; cdecl; external name 'kgpc_grad_to_deg';
function GradToRad(Grads: Real): Real; cdecl; external name 'kgpc_grad_to_rad';
function RadToGrad(Radians: Real): Real; cdecl; external name 'kgpc_rad_to_grad';
function CycleToRad(Cycles: Real): Real; cdecl; external name 'kgpc_cycle_to_rad';
function RadToCycle(Radians: Real): Real; cdecl; external name 'kgpc_rad_to_cycle';

function Max(a, b: integer): integer;
function Max(a, b: longint): longint;
function Min(a, b: integer): integer;
function Min(a, b: longint): longint;
function MaxIntValue(const Values: array of Longint): Longint;
function MinIntValue(const Values: array of Longint): Longint;
function Sum(const Values: array of Longint): Longint;
function Mean(const Values: array of Longint): Real;
function EnsureRange(Value, RangeMin, RangeMax: Longint): Longint;
function InRange(Value, RangeMin, RangeMax: Longint): Boolean;
function Sign(Value: Longint): Longint;
function Sign(Value: Real): Longint;
function Log10(Value: Real): Real;
function Log2(Value: Real): Real;
procedure SinCos(Angle: Real; var SinValue, CosValue: Real);

implementation

function Max(a, b: integer): integer;
begin
    if a >= b then
        Max := a
    else
        Max := b;
end;

function Max(a, b: longint): longint;
begin
    if a >= b then
        Max := a
    else
        Max := b;
end;

function Min(a, b: integer): integer;
begin
    if a <= b then
        Min := a
    else
        Min := b;
end;

function Min(a, b: longint): longint;
begin
    if a <= b then
        Min := a
    else
        Min := b;
end;

function MaxIntValue(const Values: array of Longint): Longint;
var
    i, start_idx, end_idx: Integer;
begin
    start_idx := Low(Values);
    end_idx := High(Values);
    if end_idx < start_idx then
    begin
        MaxIntValue := 0;
        exit;
    end;
    MaxIntValue := Values[start_idx];
    for i := start_idx + 1 to end_idx do
        if Values[i] > MaxIntValue then
            MaxIntValue := Values[i];
end;

function MinIntValue(const Values: array of Longint): Longint;
var
    i, start_idx, end_idx: Integer;
begin
    start_idx := Low(Values);
    end_idx := High(Values);
    if end_idx < start_idx then
    begin
        MinIntValue := 0;
        exit;
    end;
    MinIntValue := Values[start_idx];
    for i := start_idx + 1 to end_idx do
        if Values[i] < MinIntValue then
            MinIntValue := Values[i];
end;

function Sum(const Values: array of Longint): Longint;
var
    i, start_idx, end_idx: Integer;
begin
    start_idx := Low(Values);
    end_idx := High(Values);
    Sum := 0;
    for i := start_idx to end_idx do
        Sum := Sum + Values[i];
end;

function Mean(const Values: array of Longint): Real;
var
    total: Longint;
    count, start_idx, end_idx, i: Integer;
begin
    start_idx := Low(Values);
    end_idx := High(Values);
    if end_idx < start_idx then
    begin
        Mean := 0.0;
        exit;
    end;
    total := 0;
    count := end_idx - start_idx + 1;
    for i := start_idx to end_idx do
        total := total + Values[i];
    Mean := total / count;
end;

function EnsureRange(Value, RangeMin, RangeMax: Longint): Longint;
begin
    if RangeMin > RangeMax then
    begin
        EnsureRange := EnsureRange(Value, RangeMax, RangeMin);
        exit;
    end;
    if Value < RangeMin then
        EnsureRange := RangeMin
    else if Value > RangeMax then
        EnsureRange := RangeMax
    else
        EnsureRange := Value;
end;

function InRange(Value, RangeMin, RangeMax: Longint): Boolean;
begin
    InRange := (Value >= RangeMin) and (Value <= RangeMax);
end;

function Sign(Value: Longint): Longint;
begin
    if Value > 0 then
        Sign := 1
    else if Value < 0 then
        Sign := -1
    else
        Sign := 0;
end;

function Sign(Value: Real): Longint;
begin
    if Value > 0.0 then
        Sign := 1
    else if Value < 0.0 then
        Sign := -1
    else
        Sign := 0;
end;

function Log10(Value: Real): Real;
begin
    Log10 := Ln(Value) / Ln(10.0);
end;

function Log2(Value: Real): Real;
begin
    Log2 := Ln(Value) / Ln(2.0);
end;

procedure SinCos(Angle: Real; var SinValue, CosValue: Real);
begin
    SinValue := Sin(Angle);
    CosValue := Cos(Angle);
end;

end.
