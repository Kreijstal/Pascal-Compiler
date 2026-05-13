unit regr_inline_operator_cross_unit_helper;
{$mode objfpc}

interface

type
  TProcOpt = (
    po0, po1, po2, po3, po4, po5, po6, po7, po8, po9,
    po10, po11, po12, po13, po14, po15, po16, po17, po18, po19,
    po20, po21, po22, po23, po24, po25, po26, po27, po28, po29,
    po30, po31, po32, po33, po34, po35, po36, po37, po38, po39,
    poInline
  );
  TProcOpts = set of TProcOpt;

  TConstExprInt = record
    V: LongInt;
    Opts: TProcOpts;
  end;

operator <= (const A, B: TConstExprInt): Boolean; inline;
operator >= (const A, B: TConstExprInt): Boolean; inline;
function HasInlineOpt(const C: TConstExprInt): Boolean; inline;

implementation

operator <= (const A, B: TConstExprInt): Boolean;
begin
  Result := A.V <= B.V;
end;

operator >= (const A, B: TConstExprInt): Boolean;
begin
  Result := A.V >= B.V;
end;

function HasInlineOpt(const C: TConstExprInt): Boolean;
begin
  Result := poInline in C.Opts;
end;

end.
