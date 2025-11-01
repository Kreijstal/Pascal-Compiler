program TestArrayInRecord;

type
  TStudent = record
    StudentID: integer;
    Grades: array[1..5] of integer;
  end;

var
  student1: TStudent;

begin
  student1.Grades[3] := 95;
  writeln('The grade is: ', student1.Grades[3]);
end.
