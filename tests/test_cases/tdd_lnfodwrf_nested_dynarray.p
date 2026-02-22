{$mode objfpc}
program tdd_lnfodwrf_nested_dynarray;

{ TDD repro: array of array of record ... end
  Pattern from lnfodwrf.pp:
    Attrs : array of array of record attr,form : QWord; end;
}

type
  TAbbrevsInfo = record
    Tags: array of QWord;
    Attrs: array of array of record attr, form: QWord; end;
  end;

var
  abbrevs: TAbbrevsInfo;
begin
  SetLength(abbrevs.Tags, 4);
  SetLength(abbrevs.Attrs, 4);
  SetLength(abbrevs.Attrs[0], 2);
  abbrevs.Attrs[0][0].attr := 1;
  abbrevs.Attrs[0][0].form := 2;
  abbrevs.Attrs[0][1].attr := 3;
  abbrevs.Attrs[0][1].form := 4;
  if (abbrevs.Attrs[0][0].attr = 1) and (abbrevs.Attrs[0][1].form = 4) then
    writeln('ok')
  else
    writeln('FAIL');
end.
