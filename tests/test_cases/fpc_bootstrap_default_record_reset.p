program fpc_bootstrap_default_record_reset;

type
  ttzhead = packed record
    tzh_identifier : array[0..3] of AnsiChar;
    tzh_version : AnsiChar;
    tzh_reserved : array[0..14] of byte;
    tzh_ttisgmtcnt,
    tzh_ttisstdcnt,
    tzh_leapcnt,
    tzh_timecnt,
    tzh_typecnt,
    tzh_charcnt  : longint;
  end;

var
  tz: ttzhead;
  i: integer;
begin
  for i := 0 to 3 do
    tz.tzh_identifier[i] := Chr(65 + i); { seed with non-zero data }
  tz.tzh_version := 'X';
  tz.tzh_reserved[0] := 255;
  tz.tzh_ttisgmtcnt := 1;
  tz.tzh_ttisstdcnt := 2;
  tz.tzh_leapcnt := 3;
  tz.tzh_timecnt := 4;
  tz.tzh_typecnt := 5;
  tz.tzh_charcnt := 6;

  tz := Default(ttzhead);

  writeln(
    ord(tz.tzh_identifier[0]), ' ',
    ord(tz.tzh_version), ' ',
    tz.tzh_reserved[0], ' ',
    tz.tzh_timecnt, ' ',
    tz.tzh_charcnt
  );
end.
