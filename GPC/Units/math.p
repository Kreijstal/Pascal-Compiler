unit Math;

interface

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
procedure SinCos(Angle: Real; var SinValue, CosValue: Real);

implementation

const
    PI_VALUE = 3.14159265358979323846;

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

function Ceil(Value: Real): Longint;
var
    truncated: Longint;
begin
    truncated := Trunc(Value);
    if Value > truncated then
        Ceil := truncated + 1
    else
        Ceil := truncated;
end;

function Floor(Value: Real): Longint;
var
    truncated: Longint;
begin
    truncated := Trunc(Value);
    if (Value < 0) and (Value <> truncated) then
        Floor := truncated - 1
    else
        Floor := truncated;
end;

function DegToRad(Degrees: Real): Real;
begin
    DegToRad := Degrees * PI_VALUE / 180.0;
end;

function RadToDeg(Radians: Real): Real;
begin
    RadToDeg := Radians * 180.0 / PI_VALUE;
end;

function DegToGrad(Degrees: Real): Real;
begin
    DegToGrad := Degrees * 400.0 / 360.0;
end;

function GradToDeg(Grads: Real): Real;
begin
    GradToDeg := Grads * 360.0 / 400.0;
end;

function GradToRad(Grads: Real): Real;
begin
    GradToRad := DegToRad(GradToDeg(Grads));
end;

function RadToGrad(Radians: Real): Real;
begin
    RadToGrad := DegToGrad(RadToDeg(Radians));
end;

function CycleToRad(Cycles: Real): Real;
begin
    CycleToRad := Cycles * 2.0 * PI_VALUE;
end;

function RadToCycle(Radians: Real): Real;
begin
    RadToCycle := Radians / (2.0 * PI_VALUE);
end;

procedure SinCos(Angle: Real; var SinValue, CosValue: Real);
begin
    SinValue := sin(Angle);
    CosValue := cos(Angle);
end;

end.
