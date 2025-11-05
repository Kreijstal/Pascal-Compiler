program enum_array_index;
{ Test program to verify that enum types can be used as array indices }
type
    Month = (January, February, March, April, May, June);
    Color = (Red, Green, Blue);
const
    DaysInMonth: array [January .. June] of integer = (31, 28, 31, 30, 31, 30);
    RGBValues: array [Red .. Blue] of integer = (255, 128, 64);
var
    totalDays: integer;
    m: Month;
    c: Color;
    colorSum: integer;
begin
    { Test 1: Sum days in January and February using for loop with enum variable }
    totalDays := 0;
    for m := January to February do
        totalDays := totalDays + DaysInMonth[m];
    writeln('Days in Jan+Feb: ', totalDays);
    
    { Test 2: Calculate Q1 days using direct enum constant indexing }
    totalDays := DaysInMonth[January] + DaysInMonth[February] + DaysInMonth[March];
    writeln('Q1 total days: ', totalDays);
    
    { Test 3: Access specific months with enum constants as indices }
    writeln('Days in March: ', DaysInMonth[March]);
    writeln('Days in June: ', DaysInMonth[June]);
    
    { Test 4: Sum all months using enum variable in for loop }
    totalDays := 0;
    for m := January to June do
        totalDays := totalDays + DaysInMonth[m];
    writeln('Days Jan-Jun: ', totalDays);
    
    { Test 5: Different enum type as array index }
    colorSum := 0;
    for c := Red to Blue do
        colorSum := colorSum + RGBValues[c];
    writeln('RGB sum: ', colorSum);
    
    { Test 6: Direct access with enum constant }
    writeln('Green value: ', RGBValues[Green]);
end.
