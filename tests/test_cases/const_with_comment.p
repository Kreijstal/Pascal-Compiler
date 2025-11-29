program const_with_comment;
const
  FileMode : byte = 2;
(* This is a comment after a typed constant *)
begin
  writeln('FileMode = ', FileMode);
end.
