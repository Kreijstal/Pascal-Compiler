program tdd_explicit_enum_ord_case;

type
  TInline = (
    in_none = -1,
    in_lo_word = 1,
    in_hi_word = 2,
    in_lo_long = 3,
    in_hi_long = 4,
    in_ord_x = 5,
    in_length_x = 6,
    in_chr_byte = 7,
    in_write_x = 14,
    in_writeln_x = 15,
    in_read_x = 16,
    in_readln_x = 17,
    in_concat_x = 18,
    in_assigned_x = 19,
    in_str_x_string = 20,
    in_ofs_x = 21
  );

procedure Check(Value: TInline);
begin
  Writeln(Ord(in_writeln_x));
  Writeln(Ord(in_ofs_x));
  case Value of
    in_ofs_x:
      Writeln('ofs');
    in_write_x, in_writeln_x:
      Writeln('write');
  else
    Writeln('other');
  end;
end;

begin
  Check(in_writeln_x);
end.
