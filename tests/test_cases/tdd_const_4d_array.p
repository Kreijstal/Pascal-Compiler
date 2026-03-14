{ TDD: 4-dimensional const array
  Pattern from FPC compiler: op : array[false..true,false..true,s32real..s64real,0..3] of TAsmOp
  Error: "Unsupported 4+ dimensional const array" }
program tdd_const_4d_array;

type
  TRealType = (RT_Single, RT_Double);
  TOp = (OP_NONE, OP_ADD, OP_SUB, OP_MUL);

const
  Ops: array[Boolean, Boolean, TRealType, 0..1] of TOp = (
    (
      ((OP_ADD, OP_SUB), (OP_MUL, OP_NONE)),
      ((OP_SUB, OP_ADD), (OP_NONE, OP_MUL))
    ),
    (
      ((OP_MUL, OP_NONE), (OP_ADD, OP_SUB)),
      ((OP_NONE, OP_MUL), (OP_SUB, OP_ADD))
    )
  );

begin
  WriteLn(Ord(Ops[False, False, RT_Single, 0]));
  WriteLn(Ord(Ops[False, False, RT_Single, 1]));
  WriteLn(Ord(Ops[True, True, RT_Double, 1]));
  WriteLn(Ord(Ops[True, False, RT_Double, 0]));
end.
