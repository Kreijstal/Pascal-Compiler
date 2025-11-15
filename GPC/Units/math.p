unit Math;

interface

const
    Pi = 3.14159265358979323846;

function Sqrt(Value: Real): Real;
function Sin(Value: Real): Real;
function Cos(Value: Real): Real;
function Tan(Value: Real): Real;
function Cot(Value: Real): Real;
function Sec(Value: Real): Real;
function Csc(Value: Real): Real;
function ArcSin(Value: Real): Real;
function ArcCos(Value: Real): Real;
function ArcTan(Value: Real): Real;
function ArcTan2(Y, X: Real): Real;
function ArcCot(Value: Real): Real;
function Sinh(Value: Real): Real;
function Cosh(Value: Real): Real;
function Tanh(Value: Real): Real;
function Coth(Value: Real): Real;
function Sech(Value: Real): Real;
function Csch(Value: Real): Real;
function ArcSinh(Value: Real): Real;
function ArcCosh(Value: Real): Real;
function ArcTanh(Value: Real): Real;
function ArcCoth(Value: Real): Real;
function ArcSech(Value: Real): Real;
function ArcCsch(Value: Real): Real;
function Exp(Value: Real): Real;
function Ln(Value: Real): Real;
function LogN(Base, Value: Real): Real;
function Hypot(X, Y: Real): Real;
function Power(Base, Exponent: Real): Real;
function Round(Value: Real): Longint;
function Trunc(Value: Real): Longint;
function Int(Value: Real): Longint;
function Frac(Value: Real): Real;

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
function Ceil(Value: Real): Longint;
function Floor(Value: Real): Longint;
function DegToRad(Degrees: Real): Real;
function RadToDeg(Radians: Real): Real;
function DegToGrad(Degrees: Real): Real;
function GradToDeg(Grads: Real): Real;
function GradToRad(Grads: Real): Real;
function RadToGrad(Radians: Real): Real;
function CycleToRad(Cycles: Real): Real;
function RadToCycle(Radians: Real): Real;
procedure SinCos(Angle: Real; var SinValue, CosValue: Real); external;

implementation

const
    PI_VALUE = Pi;

function gpc_sqrt(Value: Real): Real; external;
function gpc_sin(Value: Real): Real; external;
function gpc_cos(Value: Real): Real; external;
function gpc_tan(Value: Real): Real; external;
function gpc_cot(Value: Real): Real; external;
function gpc_sec(Value: Real): Real; external;
function gpc_csc(Value: Real): Real; external;
function gpc_sinh(Value: Real): Real; external;
function gpc_cosh(Value: Real): Real; external;
function gpc_tanh(Value: Real): Real; external;
function gpc_coth(Value: Real): Real; external;
function gpc_sech(Value: Real): Real; external;
function gpc_csch(Value: Real): Real; external;
function gpc_arcsin(Value: Real): Real; external;
function gpc_arccos(Value: Real): Real; external;
function gpc_arctan(Value: Real): Real; external;
function gpc_arctan2(Y, X: Real): Real; external;
function gpc_arccot(Value: Real): Real; external;
function gpc_arcsinh(Value: Real): Real; external;
function gpc_arccosh(Value: Real): Real; external;
function gpc_arctanh(Value: Real): Real; external;
function gpc_arccoth(Value: Real): Real; external;
function gpc_arcsech(Value: Real): Real; external;
function gpc_arccsch(Value: Real): Real; external;
function gpc_exp(Value: Real): Real; external;
function gpc_ln(Value: Real): Real; external;
function gpc_logn(Base, Value: Real): Real; external;
function gpc_hypot(X, Y: Real): Real; external;
function gpc_power(Base, Exponent: Real): Real; external;
function gpc_round(Value: Real): int64; external;
function gpc_trunc(Value: Real): int64; external;
function gpc_int(Value: Real): int64; external;
function gpc_frac(Value: Real): Real; external;
function gpc_ceil(Value: Real): int64; external;
function gpc_floor(Value: Real): int64; external;
function gpc_deg_to_rad(Value: Real): Real; external;
function gpc_rad_to_deg(Value: Real): Real; external;
function gpc_deg_to_grad(Value: Real): Real; external;
function gpc_grad_to_deg(Value: Real): Real; external;
function gpc_grad_to_rad(Value: Real): Real; external;
function gpc_rad_to_grad(Value: Real): Real; external;
function gpc_cycle_to_rad(Value: Real): Real; external;
function gpc_rad_to_cycle(Value: Real): Real; external;

function Sqrt(Value: Real): Real;
begin
    Sqrt := gpc_sqrt(Value);
end;

function Sin(Value: Real): Real;
begin
    Sin := gpc_sin(Value);
end;

function Cos(Value: Real): Real;
begin
    Cos := gpc_cos(Value);
end;

function Tan(Value: Real): Real;
begin
    Tan := gpc_tan(Value);
end;

function Cot(Value: Real): Real;
begin
    Cot := gpc_cot(Value);
end;

function Sec(Value: Real): Real;
begin
    Sec := gpc_sec(Value);
end;

function Csc(Value: Real): Real;
begin
    Csc := gpc_csc(Value);
end;

function ArcSin(Value: Real): Real;
begin
    ArcSin := gpc_arcsin(Value);
end;

function ArcCos(Value: Real): Real;
begin
    ArcCos := gpc_arccos(Value);
end;

function ArcTan(Value: Real): Real;
begin
    ArcTan := gpc_arctan(Value);
end;

function ArcTan2(Y, X: Real): Real;
begin
    ArcTan2 := gpc_arctan2(Y, X);
end;

function ArcCot(Value: Real): Real;
begin
    ArcCot := gpc_arccot(Value);
end;

function Sinh(Value: Real): Real;
begin
    Sinh := gpc_sinh(Value);
end;

function Cosh(Value: Real): Real;
begin
    Cosh := gpc_cosh(Value);
end;

function Tanh(Value: Real): Real;
begin
    Tanh := gpc_tanh(Value);
end;

function Coth(Value: Real): Real;
begin
    Coth := gpc_coth(Value);
end;

function Sech(Value: Real): Real;
begin
    Sech := gpc_sech(Value);
end;

function Csch(Value: Real): Real;
begin
    Csch := gpc_csch(Value);
end;

function ArcSinh(Value: Real): Real;
begin
    ArcSinh := gpc_arcsinh(Value);
end;

function ArcCosh(Value: Real): Real;
begin
    ArcCosh := gpc_arccosh(Value);
end;

function ArcTanh(Value: Real): Real;
begin
    ArcTanh := gpc_arctanh(Value);
end;

function ArcCoth(Value: Real): Real;
begin
    ArcCoth := gpc_arccoth(Value);
end;

function ArcSech(Value: Real): Real;
begin
    ArcSech := gpc_arcsech(Value);
end;

function ArcCsch(Value: Real): Real;
begin
    ArcCsch := gpc_arccsch(Value);
end;

function Exp(Value: Real): Real;
begin
    Exp := gpc_exp(Value);
end;

function Ln(Value: Real): Real;
begin
    Ln := gpc_ln(Value);
end;

function LogN(Base, Value: Real): Real;
begin
    LogN := gpc_logn(Base, Value);
end;

function Hypot(X, Y: Real): Real;
begin
    Hypot := gpc_hypot(X, Y);
end;

function Power(Base, Exponent: Real): Real;
begin
    Power := gpc_power(Base, Exponent);
end;

function Round(Value: Real): Longint;
begin
    Round := Longint(gpc_round(Value));
end;

function Trunc(Value: Real): Longint;
begin
    Trunc := Longint(gpc_trunc(Value));
end;

function Int(Value: Real): Longint;
begin
    Int := Longint(gpc_int(Value));
end;

function Frac(Value: Real): Real;
begin
    Frac := gpc_frac(Value);
end;

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
    if RangeMin > RangeMax then
        InRange := (Value >= RangeMax) and (Value <= RangeMin)
    else
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

function Ceil(Value: Real): Longint;
begin
    Ceil := Longint(gpc_ceil(Value));
end;

function Floor(Value: Real): Longint;
begin
    Floor := Longint(gpc_floor(Value));
end;

function DegToRad(Degrees: Real): Real;
begin
    DegToRad := gpc_deg_to_rad(Degrees);
end;

function RadToDeg(Radians: Real): Real;
begin
    RadToDeg := gpc_rad_to_deg(Radians);
end;

function DegToGrad(Degrees: Real): Real;
begin
    DegToGrad := gpc_deg_to_grad(Degrees);
end;

function GradToDeg(Grads: Real): Real;
begin
    GradToDeg := gpc_grad_to_deg(Grads);
end;

function GradToRad(Grads: Real): Real;
begin
    GradToRad := gpc_grad_to_rad(Grads);
end;

function RadToGrad(Radians: Real): Real;
begin
    RadToGrad := gpc_rad_to_grad(Radians);
end;

function CycleToRad(Cycles: Real): Real;
begin
    CycleToRad := gpc_cycle_to_rad(Cycles);
end;

function RadToCycle(Radians: Real): Real;
begin
    RadToCycle := gpc_rad_to_cycle(Radians);
end;

end.
