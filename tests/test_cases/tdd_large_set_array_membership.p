program tdd_large_set_array_membership;

{$mode objfpc}

type
  TOperatorPrecedence = (opcompare, opaddition, opmultiply, oppower);
  TToken = (
    NOTOKEN, _PLUS, _MINUS, _STAR, _SLASH, _EQ, _GT, _LT, _GTE, _LTE, _NE,
    _SYMDIF, _STARSTAR, _OP_AS, _OP_IN, _OP_IS, _OP_OR, _OP_AND, _OP_DIV,
    _OP_MOD, _OP_NOT, _OP_SHL, _OP_SHR, _OP_XOR, _ASSIGNMENT, _OP_EXPLICIT,
    _OP_ENUMERATOR, _OP_INITIALIZE, _OP_FINALIZE, _OP_ADDREF, _OP_COPY,
    _OP_INC, _OP_DEC, _CARET, _LECKKLAMMER, _RECKKLAMMER, _POINT, _COMMA,
    _LKLAMMER, _RKLAMMER, _COLON, _SEMICOLON, _KLAMMERAFFE, _POINTPOINT,
    _POINTPOINTPOINT, _PIPE, _AMPERSAND, _EOF, _ID, _NOID, _REALNUMBER,
    _INTCONST, _CSTRING, _CCHAR, _CWSTRING, _CWCHAR, _LSHARPBRACKET,
    _RSHARPBRACKET, _PLUSASN, _MINUSASN, _ANDASN, _ORASN, _STARASN,
    _SLASHASN, _MODASN, _DIVASN, _NOTASN, _XORASN, _GENERICSPECIALTOKEN);

const
  LastOperator = _GENERICSPECIALTOKEN;
  OperatorLevels: array[TOperatorPrecedence] of set of NOTOKEN..LastOperator =
    ([_LT, _LTE, _GT, _GTE, _EQ, _NE, _OP_IN, _OP_IS],
     [_PLUS, _MINUS, _OP_OR, _PIPE, _OP_XOR],
     [_CARET, _SYMDIF, _STARSTAR, _STAR, _SLASH,
      _OP_AS, _OP_AND, _AMPERSAND, _OP_DIV, _OP_MOD, _OP_SHL, _OP_SHR],
     [_STARSTAR]);

var
  Token: TToken;

begin
  Token := _POINT;
  if Token in OperatorLevels[opmultiply] then
    Writeln('bad')
  else if not (_STARSTAR in OperatorLevels[oppower]) then
    Writeln('missing')
  else
    Writeln('ok');
end.
