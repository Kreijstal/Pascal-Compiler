{ 
  Regression test: Stray identifier before begin block should cause a parse error.
  This tests that the parser properly rejects invalid content between declaration
  sections and the main block.
  
  Previously, 'rot' would be silently ignored and the program would compile
  but produce no output.
}
program colors;
const cyan=#27+'[36m';
clear=#27+'[0m';
rot
begin
        writeln('Hello ',cyan,'World',clear);
end.
