program SemcheckFullRepro;

  const
    MaximalAlfa = 20;
    MaximalIdentifiers = 1;

  type
    TAlfa = array[1..MaximalAlfa] of char;

    TIdent = record
      Inside: boolean;
    end;

  procedure StringCopy(var Dest: TAlfa; Src: TAlfa);
  begin
  end;

  procedure EmitChar(c: char);
  begin
  end;

  var
    Keywords: array[1..1] of TAlfa;
    Identifiers: array[0..MaximalIdentifiers] of TIdent;
    i: integer;

  begin
    i := 0;
    StringCopy(Keywords[1], 'BEGIN               ');
    EmitChar(chr(1));
    if not Identifiers[i].Inside then
    begin
    end;
  end.
