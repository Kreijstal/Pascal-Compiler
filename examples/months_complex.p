program months_complex;

type
    Month = (January, February, March, April, May,
             June, July, August, September,
             October, November, December);
    MonthSet = set of Month;
    EachMonth = record
         Name, Short: string;
         Count: integer;
    end;

    complex = record
        R, I: real;
    end;

const
     Months: array [January .. December] of EachMonth = (
       (Name: 'January';   Short: 'Jan'; Count: 31),
       (Name: 'February';  Short: 'Feb'; Count: 28),
       (Name: 'March';     Short: 'Mar'; Count: 31),
       (Name: 'April';     Short: 'Apr'; Count: 30),
       (Name: 'May';       Short: 'May'; Count: 31),
       (Name: 'June';      Short: 'Jun'; Count: 30),
       (Name: 'July';      Short: 'Jul'; Count: 31),
       (Name: 'August';    Short: 'Aug'; Count: 31),
       (Name: 'September'; Short: 'Sep'; Count: 30),
       (Name: 'October';   Short: 'Oct'; Count: 31),
       (Name: 'November';  Short: 'Nov'; Count: 30),
       (Name: 'December';  Short: 'Dec'; Count: 31)
     );
     Pi = 3.14159267;
     C2: complex = ( R: 96; I:1.62);
     C_Low = 1; C_High = 5;
     C: array [C_low .. C_high] of complex = (
         (R:3; I:1783.5),
         (R:96; I:1.62),
         (R:17; I:115),
         (R:1.9e56; I:72.43),
         (R:102.1; I:Pi)
     );

var
    m: Month;
    idx: integer;
    totalDays: integer;
    firstQuarter: MonthSet;
    value: complex;

begin
    totalDays := 0;
    for m := January to December do
        totalDays := totalDays + Months[m].Count;
    writeln('Total days accounted for: ', totalDays);

    firstQuarter := [January, February, March, April];
    writeln('First quarter months:');
    for m := January to April do
        if m in firstQuarter then
            writeln('  ', Months[m].Name, ' (', Months[m].Short, '): ', Months[m].Count, ' days');

    writeln('Complex constants:');
    for idx := C_Low to C_High do
    begin
        value := C[idx];
        writeln('  C[', idx, '] = (', value.R:0:2, ', ', value.I:0:2, ')');
    end;

    writeln('Reference complex value C2: (', C2.R:0:2, ', ', C2.I:0:2, ')');
end.
